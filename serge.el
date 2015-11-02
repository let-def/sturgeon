(require 'tq)

(defvar-local serge--process nil "The serge process for this buffer.")

(defvar-local serge--process-queue nil
  "The transaction queue for the local process (only valid in a process buffer).")

(defcustom serge-debug nil
  "Dump I/O to message buffer")

(defun serge--debug (prefix content)
  (when serge-debug
    (message "%s %s" prefix content)))

;; COMMUNICATION

(defvar-local serge--counter 0 "Gensym facility for generating addresse")

(defun serge--gensym ()
  (setq serge--counter (1+ serge--counter))
  serge--counter)

(defvar-local serge--dispatch-table nil "Table storing addresses to dispatch on")
(defvar-local serge-query-handler 'serge--cancel-high
  "Function invoked when receiving query")

(defun serge--dispatch-table ()
  (unless serge--dispatch-table
    (setq serge--dispatch-table (make-hash-table)))
  serge--dispatch-table)

(defun serge--dispatch-register (obj)
  (let ((key (serge--gensym)))
    (puthash key obj (serge--dispatch-table))
    (cons (car obj) key)))

;; MANAGE ENHANCED SEXP

(defun serge--sym-is-action (sym)
  (or (eq sym 'once) (eq sym 'sink)))

(defun serge--cancel-high (sexp)
  (cond
   ((and (serge--sym-is-action (car-safe sexp)) 
         (functionp (cdr-safe sexp)))
    (with-demoted-errors "cancelling closure %S"
      ((cdr-safe exp) '(abort . cancel))))
   ((eq (car-safe sexp) 'meta) nil)
   ((consp sexp)
    (serge--cancel-high (car sexp))
    (serge--cancel-high (cdr sexp)))
   (t nil)))

(defun serge--lower (sexp)
  (cond
   ((and (serge--sym-is-action (car-safe sexp)) 
         (functionp (cdr-safe sexp)))
    (cons 'meta (serge--dispatch-register sexp)))
   ((eq (car-safe sexp) 'meta)
    (cons 'meta (cons 'escape (cdr-safe sexp))))
   ((consp sexp)
    (cons (serge--lower (car sexp))
          (serge--lower (cdr sexp))))
   (t sexp)))

(defun serge--lift (kind addr)
  (lexical-let ((addr addr) (state t))
    (cons kind
          (lambda (v) (cond
                       ((not state)
                        (serge--cancel-high v))
                       ((eq v 'close)
                        (setq state nil)
                        (serge--send (cons 'close addr)))
                       ((eq (car-safe v) 'abort)
                        (setq state nil)
                        (serge--send (cons 'abort (cons addr (cdr v)))))
                       (t
                        (when (eq kind 'one) (setq state nil))
                        (serge--send (cons 'feed (cons addr (serge--lower v)))))
                       )))))

(defun serge--cancel-low (sexp)
  (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    nil)
   ((and (eq (car-safe sexp) 'meta)
         (serge--sym-is-action (car-safe (cdr sexp))))
    (serge--send (cons 'abort (cons (cddr sexp) 'cancel))))
   ((consp sexp)
    (serge--cancel-low (car sexp))
    (serge--cancel-low (cdr sexp)))
   (t nil)))

(defun serge--higher (sexp)
  (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    (cons 'meta (cddr sexp)))
   ((and (eq (car-safe sexp) 'meta)
         (serge--sym-is-action (car-safe (cdr sexp))))
    (serge--lift (cadr sexp) (cddr sexp)))
   ((consp sexp)
    (cons (serge--lower (car sexp))
          (serge--lower (cdr sexp))))
   (t sexp)))

;; PROCESS MANAGEMENT

(defun serge--wake-up (addr kind payload)
  (let (handler (gethash addr (serge--dispatch-table)))
    (when (or (eq 'once (car handler))
              (eq 'abort kind)
              (eq 'close kind))
      (remhash addr (serge--dispatch-table)))
    ((cdr handler) kind payload)))

(defun serge--handler (answer)
  (setq answer (car (read-from-string answer)))
  (let ((cmd (car-safe answer))
        (addr (car-safe (cdr-safe answer)))
        (payload (cdr-safe (cdr-safe answer))))
    (cond
     ((eq cmd 'query)
      (funcall serge-query-handler (serge--higher payload)))
     ((or (eq cmd 'feed) (eq cmd 'abort) (eq cmd 'close))
      (with-demoted-errors "wakeup %S"
        (when (eq cmd 'feed)
          (setq payload (serge--higher payload)))
        (serge--wake-up addr cmd payload)))
     ((eq answer 'end)
      ;; FIXME
      t)
     (t (serge--cancel-low answer)))))

(defun serge--resident-handler (closure answer)
  (serge--debug ">" answer)
  (tq-enqueue serge--process-queue "" "\n" nil #'serge--resident-handler)
  (serge--handler answer))

(defun serge--send (command)
  (setq command (prin1-to-string command))
  (serge--debug "<" command)
  (process-send-string serge--process command)
  (process-send-string serge--process "\n"))

(defun serge--start-process (args)
  "Collection of workarounds for starting process"
  (when (and serge--process (equal (process-status serge--process) 'run))
    (with-demoted-errors "serge--start-process: %S"
      (tq-close merlin-process-queue)
      (kill-process merlin-process)))
  (let* ((start-file-process
          (if (boundp 'start-file-process)
              #'start-file-process
            #'start-process))
         (process-connection-type nil))
    (setq serge--process (apply start-file-process args))
    (setq serge--process-queue (tq-create serge--process))
    (tq-enqueue serge--process-queue "" "\n" nil
                #'serge--resident-handler)))

(provide 'serge)

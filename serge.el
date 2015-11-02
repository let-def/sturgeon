(defcustom serge-debug nil
  "Dump I/O to message buffer")

(defun serge--debug (prefix content)
  (when serge-debug
    (message "%s %s" prefix content)))

(defun serge--filter (proc lines)
  (setq lines (split-string lines "\n"))
  (if (not (cdr lines))
      (process-put proc 'serge-lines
                   (cons (car lines) (process-get proc 'serge-lines)))
    (setcar lines (mapconcat 'identity
                             (reverse (cons (car lines)
                                            (process-get proc 'serge-lines)))
                             "\n"))
    (let ((lines lines))
      (while (cdr (cdr lines)) (setq lines (cdr lines)))
      (process-put proc 'serge-lines (cdr lines))
      (setcdr lines nil))
    (dolist (line lines)
      (with-demoted-errors "reading serge input: %S"
        (serge--debug ">" line)
        (serge--handler proc line)))))

;; COMMUNICATION

(defun serge--register (process obj)
  ;; gensym
  (let* ((table (process-get process 'serge-table))
         (key (car table)))
    (setcar table (1+ key))
    (puthash key obj (cdr table))
    (cons (car obj) key)))

;; MANAGE ENHANCED SEXP

(defun serge--sym-is-action (sym)
  (or (eq sym 'once) (eq sym 'sink)))

(defun serge--cancel-high (sexp)
  (message "cancelling %S" sexp)
  (cond
   ((and (serge--sym-is-action (car-safe sexp)) 
         (functionp (cdr-safe sexp)))
    (with-demoted-errors "cancelling closure %S"
      (funcall (cdr-safe sexp) '(abort . cancel))))
   ((eq (car-safe sexp) 'meta) nil)
   ((consp sexp)
    (serge--cancel-high (car sexp))
    (serge--cancel-high (cdr sexp)))
   (t nil)))

(defun serge--lower (process sexp)
  (cond
   ((and (serge--sym-is-action (car-safe sexp)) 
         (functionp (cdr-safe sexp)))
    (cons 'meta (serge--register process sexp)))
   ((eq (car-safe sexp) 'meta)
    (cons 'meta (cons 'escape (cdr-safe sexp))))
   ((consp sexp)
    (cons (serge--lower process (car sexp))
          (serge--lower process (cdr sexp))))
   (t sexp)))

(defun serge--lift (process kind addr)
  (lexical-let ((addr addr) (state t) (process process) (kind kind))
    (cons
     kind
     (lambda (v)
       (cond
        ((not state)
         (serge--cancel-high v))
        ((eq v 'close)
         (setq state nil)
         (serge--send process (cons 'close addr)))
        ((eq (car-safe v) 'abort)
         (setq state nil)
         (serge--send process (cons 'abort (cons addr (cdr v)))))
        (t
         (when (eq kind 'one) (setq state nil))
         (serge--send process (cons 'feed (cons addr (serge--lower process v)))))
        )))))

(defun serge--cancel-low (process sexp)
  (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    nil)
   ((and (eq (car-safe sexp) 'meta)
         (serge--sym-is-action (car-safe (cdr sexp))))
    (serge--send process (cons 'abort (cons (cddr sexp) 'cancel))))
   ((consp sexp)
    (serge--cancel-low process (car sexp))
    (serge--cancel-low process (cdr sexp)))
   (t nil)))

(defun serge--higher (process sexp)
  (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    (cons 'meta (cddr sexp)))
   ((and (eq (car-safe sexp) 'meta)
         (serge--sym-is-action (car-safe (cdr sexp))))
    (serge--lift process (cadr sexp) (cddr sexp)))
   ((consp sexp)
    (cons (serge--higher process (car sexp))
          (serge--higher process (cdr sexp))))
   (t sexp)))

;; PROCESS MANAGEMENT

(defun serge--wake-up (process addr kind payload)
  (let* ((table (process-get process 'serge-table))
         (handler (gethash addr (cdr table)))
         (fn (cdr handler)))
    (when (or (eq 'once (car handler))
              (member kind '(abort close)))
      (remhash addr (cdr table)))
    (funcall fn kind payload)))

(defun serge--handler (process answer)
  (setq answer (car (read-from-string answer)))
  (let ((cmd (car-safe answer))
        (payload (cdr-safe answer)))
    (cond
     ((eq cmd 'query)
      (funcall (process-get process 'serge-handler)
               (serge--higher process (cdr-safe answer))))
     ((eq cmd 'feed)
      (with-demoted-errors "feed %S"
        (serge--wake-up process
                        (car payload) 'feed
                        (serge--higher process (cdr payload)))))
     ((eq cmd 'abort)
      (with-demoted-errors "abort %S"
        (serge--wake-up process
                        (car payload) 'abort (cdr payload))))
     ((eq cmd 'close)
      (with-demoted-errors "close %S"
        (serge--wake-up process payload 'close nil)))
     ((eq answer 'end)
      ;; FIXME
      t)
     (t (serge--cancel-low process answer)))))

(defun serge--send (process command)
  (setq command (prin1-to-string command))
  (serge--debug "<" command)
  (process-send-string process command)
  (process-send-string process "\n"))

(defun serge-start (process &optional handler)
  (process-put process 'serge-lines nil)
  (process-put process 'serge-handler (or handler #'serge--cancel-high))
  (process-put process 'serge-table (cons 0 (make-hash-table)))
  (set-process-filter process #'serge--filter)
  process)

(defun serge-query (process query)
  (serge--send process (cons 'query (serge--lower process query))))

(defun serge-start-process (name buffer path args &optional handler)
  (let* ((start-file-process
          (if (fboundp 'start-file-process)
              #'start-file-process
            #'start-process))
         (process-connection-type nil)
         (process (apply start-file-process name buffer path args)))
    (serge-start process handler)))

(provide 'serge)

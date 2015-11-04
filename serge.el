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

;; FACILITIES FOR WORKING WITH NEGATIONS

(defmacro lambda-once (args &rest body)
  `(cons 'once (lambda ,args ,@body)))

(defmacro lambda-sink (args &rest body)
  `(cons 'sink (lambda ,args ,@body)))

(put 'lambda-once 'lisp-indent-function 'defun)
(put 'lambda-sink 'lisp-indent-function 'defun)

(defun serge--app-p (sexp)
  (and (consp sexp)
       (functionp (cdr sexp))
       (member (car sexp) '(once sink))))

(defun app-once (f arg)
  (assert (serge--app-p f))
  (assert (eq (car f) 'once))
  (funcall (cdr f) 'feed arg))
  
(defun app-sink (f arg)
  (assert (serge--app-p f))
  (assert (eq (car f) 'sink))
  (funcall (cdr f) 'feed arg))

(defun app-any (f arg)
  (assert (serge--app-p f))
  (funcall (cdr f) 'feed arg))

(defun app-quit (f &optional arg)
  (assert (serge--app-p f))
  (funcall (cdr f) 'quit arg))

;; GARBAGE COLLECTION

(defvar serge--processes nil)

(defun serge--root-register (process addr value)
  (let* ((roots (process-get process 'serge-roots))
           (addrs (car roots))
           (weaks (cdr roots)))
    (puthash addr t     addrs)
    (puthash addr value weaks)
    value))

(defun serge--root-alive (process addr)
  (gethash addr (car (process-get process 'serge-roots))))

(defun serge--root-remove (process addr)
  (let* ((roots (process-get process 'serge-roots))
         (addrs (car roots))
         (weaks (cdr roots)))
    (remhash addr addrs)
    (remhash addr weaks)))

(defun serge--collect-roots ()
  (dolist (process serge--processes)
    (let* ((roots (process-get process 'serge-roots))
           (addrs (car roots))
           (weaks (cdr roots)))
      (maphash
       (lambda (addr v)
         (unless (gethash addr weaks)
           (remhash addr addrs)
           (ignore-errors
             (serge--send process (cons 'quit (cons addr 'finalize))))))
       addrs))))

(defun serge--gc-hook ()
  (setq serge--processes
        (delete-if (lambda (process)
                     (member (process-status process)
                             '(exit signal closed failed nil)))
                   serge--processes))
  (run-at-time 0 nil #'serge--collect-roots))

(add-hook 'post-gc-hook 'serge--gc-hook)

;; COMMUNICATION

(defun serge--register (process obj)
  ;; gensym
  (let* ((table (process-get process 'serge-table))
         (key (car table)))
    (setcar table (1+ key))
    (puthash key obj (cdr table))
    (cons (car obj) key)))

;; MANAGE ENHANCED SEXP

(defun serge-cancel (sexp)
  (message "cancelling %S" sexp)
  (cond
   ((serge--app-p sexp)
    (with-demoted-errors "cancelling closure %S"
      (app-quit sexp 'cancel)))
   ((eq (car-safe sexp) 'meta) nil)
   ((consp sexp)
    (serge-cancel (car sexp))
    (serge-cancel (cdr sexp)))
   (t nil)))

(defun serge--lower (process sexp)
  (cond
   ((serge--app-p sexp)
    (cons 'meta (serge--register process sexp)))
   ((eq (car-safe sexp) 'meta)
    (cons 'meta (cons 'escape (cdr-safe sexp))))
   ((consp sexp)
    (cons (serge--lower process (car sexp))
          (serge--lower process (cdr sexp))))
   (t sexp)))

(defun serge--lift (process kind addr)
  (lexical-let ((addr addr) (process process) (kind kind))
    (cons
     kind
     (serge--root-register
      process addr
      (lambda (msg v)
        (cond
         ((not (serge--root-alive process addr))
          (serge-cancel v))
         ((eq msg 'quit)
          (serge--root-remove process addr)
          (serge--send process (cons 'quit (cons addr v))))
         (t
          (when (eq kind 'once) (serge--root-remove process addr))
          (serge--send process (cons 'feed (cons addr (serge--lower process v)))))
         ))))))

(defun serge--cancel-low (process sexp) (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    nil)
   ((and (eq (car-safe sexp) 'meta)
         (member (car-safe (cdr sexp)) '(once sink)))
    (serge--send process (cons 'quit (cons (cddr sexp) 'cancel))))
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
         (member (car-safe (cdr sexp)) '(once sink)))
    (serge--lift process (cadr sexp) (cddr sexp)))
   ((consp sexp)
    (cons (serge--higher process (car sexp))
          (serge--higher process (cdr sexp))))
   (t sexp)))

;; PROCESS MANAGEMENT

(defun serge--wake-up (process addr msg payload)
  (let* ((table (process-get process 'serge-table))
         (handler (gethash addr (cdr table)))
         (fn (cdr handler)))
    (when (or (eq 'once (car handler)) (eq msg 'quit))
      (remhash addr (cdr table)))
    (funcall fn msg payload)))

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
     ((eq cmd 'quit)
      (with-demoted-errors "quit %S"
        (serge--wake-up process
                        (car payload) 'quit (cdr payload))))

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
  (if (process-get process 'serge-handler)
      (process-put process 'serge-handler (or handler #'serge-cancel))
    (process-put process 'serge-lines nil)
    (process-put process 'serge-handler (or handler #'serge-cancel))
    (process-put process 'serge-table (cons 0 (make-hash-table)))
    (process-put process 'serge-roots
                 (cons (make-hash-table)
                       (make-hash-table :weakness 'value)))
    (set-process-filter process #'serge--filter)
    (setq serge--processes (cons process serge--processes)))
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

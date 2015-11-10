(defcustom sturgeon-debug nil
  "Dump I/O to message buffer")

(defun sturgeon--debug (prefix content)
  (when sturgeon-debug
    (message "%s %s" prefix content)))

(defun sturgeon--filter (proc lines)
  (setq lines (split-string lines "\n"))
  (unless (cdr lines)
    (process-put proc 'sturgeon-lines
                 (cons (car lines) (process-get proc 'sturgeon-lines))))
  (when (cdr lines)
    (let ((line (cons (car lines) (process-get proc 'sturgeon-lines))))
      (setcar lines (apply 'concat (reverse line))))
    (let ((lines lines))
      (while (cdr (cdr lines)) (setq lines (cdr lines)))
      (process-put proc 'sturgeon-lines (cdr lines))
      (setcdr lines nil))
    (dolist (line lines)
      (with-demoted-errors "reading sturgeon input: %S"
        (sturgeon--debug ">" line)
        (sturgeon--handler proc line)))))

;; Routines for working with negations

(defmacro lambda-once (args &rest body)
  `(cons 'once (lambda ,args ,@body)))

(defmacro lambda-sink (args &rest body)
  `(cons 'sink (lambda ,args ,@body)))

(put 'lambda-once 'lisp-indent-function 'defun)
(put 'lambda-sink 'lisp-indent-function 'defun)

(defun sturgeon--app-p (sexp)
  (and (consp sexp)
       (functionp (cdr sexp))
       (member (car sexp) '(once sink))))

(defun app-once (f arg)
  (assert (sturgeon--app-p f))
  (assert (eq (car f) 'once))
  (funcall (cdr f) 'feed arg))

(defun app-sink (f arg)
  (assert (sturgeon--app-p f))
  (assert (eq (car f) 'sink))
  (funcall (cdr f) 'feed arg))

(defun app-any (f arg)
  (assert (sturgeon--app-p f))
  (funcall (cdr f) 'feed arg))

(defun app-quit (f &optional arg)
  (assert (sturgeon--app-p f))
  (funcall (cdr f) 'quit arg))

;; Garbage collection

(defvar sturgeon--processes nil)

(defun sturgeon--root-register (process addr value)
  (let* ((roots (process-get process 'sturgeon-roots))
           (addrs (car roots))
           (weaks (cdr roots)))
    (puthash addr t     addrs)
    (puthash addr value weaks)
    value))

(defun sturgeon--root-alive (process addr)
  (gethash addr (car (process-get process 'sturgeon-roots))))

(defun sturgeon--root-remove (process addr)
  (let* ((roots (process-get process 'sturgeon-roots))
         (addrs (car roots))
         (weaks (cdr roots)))
    (remhash addr addrs)
    (remhash addr weaks)))

(defun sturgeon--collect-roots ()
  (dolist (process sturgeon--processes)
    (let* ((roots (process-get process 'sturgeon-roots))
           (addrs (car roots))
           (weaks (cdr roots)))
      (maphash
       (lambda (addr v)
         (unless (gethash addr weaks)
           (remhash addr addrs)
           (ignore-errors
             (sturgeon--send process (cons 'quit (cons addr 'finalize))))))
       addrs))))

(defun sturgeon--gc-hook ()
  (setq sturgeon--processes
        (delete-if (lambda (process)
                     (member (process-status process)
                             '(exit signal closed failed nil)))
                   sturgeon--processes))
  (run-at-time 0 nil #'sturgeon--collect-roots))

(add-hook 'post-gc-hook 'sturgeon--gc-hook)

;; Communication -- convert to and from extended s-exps

(defun sturgeon--register (process obj)
  ;; gensym
  (let* ((table (process-get process 'sturgeon-table))
         (key (car table)))
    (setcar table (1+ key))
    (puthash key obj (cdr table))
    (cons (car obj) key)))

(defun sturgeon-cancel (sexp)
  (message "cancelling %S" sexp)
  (cond
   ((sturgeon--app-p sexp)
    (with-demoted-errors "cancelling closure %S"
      (app-quit sexp 'cancel)))
   ((eq (car-safe sexp) 'meta) nil)
   ((consp sexp)
    (sturgeon-cancel (car sexp))
    (sturgeon-cancel (cdr sexp)))
   (t nil)))

(defun sturgeon--lower (process sexp)
  (cond
   ((sturgeon--app-p sexp)
    (cons 'meta (sturgeon--register process sexp)))
   ((eq (car-safe sexp) 'meta)
    (cons 'meta (cons 'escape (cdr-safe sexp))))
   ((consp sexp)
    (cons (sturgeon--lower process (car sexp))
          (sturgeon--lower process (cdr sexp))))
   (t sexp)))

(defun sturgeon--lift (process kind addr)
  (lexical-let ((addr addr) (process process) (kind kind))
    (cons
     kind
     (sturgeon--root-register
      process addr
      (lambda (msg v)
        (cond
         ((not (sturgeon--root-alive process addr))
          (sturgeon-cancel v))
         ((eq msg 'quit)
          (sturgeon--root-remove process addr)
          (sturgeon--send process (cons 'quit (cons addr v))))
         (t
          (when (eq kind 'once) (sturgeon--root-remove process addr))
          (sturgeon--send process (cons 'feed (cons addr (sturgeon--lower process v)))))
         ))))))

(defun sturgeon--cancel-low (process sexp) (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    nil)
   ((and (eq (car-safe sexp) 'meta)
         (member (car-safe (cdr sexp)) '(once sink)))
    (sturgeon--send process (cons 'quit (cons (cddr sexp) 'cancel))))
   ((consp sexp)
    (sturgeon--cancel-low process (car sexp))
    (sturgeon--cancel-low process (cdr sexp)))
   (t nil)))

(defun sturgeon--higher (process sexp)
  (cond
   ((and (eq (car-safe sexp) 'meta)
         (eq (car-safe (cdr sexp)) 'escape))
    (cons 'meta (cddr sexp)))
   ((and (eq (car-safe sexp) 'meta)
         (member (car-safe (cdr sexp)) '(once sink)))
    (sturgeon--lift process (cadr sexp) (cddr sexp)))
   ((consp sexp)
    (cons (sturgeon--higher process (car sexp))
          (sturgeon--higher process (cdr sexp))))
   (t sexp)))

;; Process management

(defun sturgeon--wake-up (process addr msg payload)
  (let* ((table (process-get process 'sturgeon-table))
         (handler (gethash addr (cdr table)))
         (fn (cdr handler)))
    (when (or (eq 'once (car handler)) (eq msg 'quit))
      (remhash addr (cdr table)))
    (funcall fn msg payload)))

(defun sturgeon--handler (process answer)
  (setq answer (car (read-from-string answer)))
  (let ((cmd (car-safe answer))
        (payload (cdr-safe answer)))
    (cond
     ((eq cmd 'query)
      (funcall (process-get process 'sturgeon-handler)
               (sturgeon--higher process (cdr-safe answer))))
     ((eq cmd 'feed)
      (with-demoted-errors "feed %S"
        (sturgeon--wake-up process
                        (car payload) 'feed
                        (sturgeon--higher process (cdr payload)))))
     ((eq cmd 'quit)
      (with-demoted-errors "quit %S"
        (sturgeon--wake-up process
                        (car payload) 'quit (cdr payload))))

     ((eq answer 'end)
      ;; FIXME
      t)
     (t (sturgeon--cancel-low process answer)))))

(defun sturgeon--send (process command)
  (setq command (prin1-to-string command))
  (sturgeon--debug "<" command)
  (process-send-string process command)
  (process-send-string process "\n"))

;; Main functions

(defun sturgeon-start (process &optional handler)
  (if (process-get process 'sturgeon-handler)
      (process-put process 'sturgeon-handler (or handler #'sturgeon-cancel))
    (process-put process 'sturgeon-lines nil)
    (process-put process 'sturgeon-handler (or handler #'sturgeon-cancel))
    (process-put process 'sturgeon-table (cons 0 (make-hash-table)))
    (process-put process 'sturgeon-roots
                 (cons (make-hash-table)
                       (make-hash-table :weakness 'value)))
    (set-process-filter process #'sturgeon--filter)
    (setq sturgeon--processes (cons process sturgeon--processes)))
  process)

(defun sturgeon-query (process query)
  (sturgeon--send process (cons 'query (sturgeon--lower process query))))

(defun sturgeon-start-process (name buffer path args &optional handler)
  (let* ((start-file-process
          (if (fboundp 'start-file-process)
              #'start-file-process
            #'start-process))
         (process-connection-type nil)
         (process (apply start-file-process name buffer path args)))
    (sturgeon-start process handler)))

;; Rich printing facility

(require 'button)

(defun sturgeon-ui--cursor-action (x)
  (let* ((action (button-get x 'sturgeon-sink))
         (marker (button-get x 'sturgeon-marker))
         (offset (- (marker-position x) (marker-position marker))))
    (app-sink action (cons 'click (1+ offset)))))

(defun sturgeon-ui--make-cursor (buffer point sink)
  (lexical-let ((buffer buffer) (sink sink) (marker (make-marker)))
    (set-marker marker point buffer)
    (set-marker-insertion-type marker t)
    (lambda-sink (kind value)
      ;; (when (eq kind 'quit) ...)
      (when (eq kind 'feed)
        (cond
         ;; Clear sub regions
         ((eq (car value) 'substitute)
          (let ((start  (+ (marker-position marker) (cadr value)))
                (length (caddr value))
                (text   (cadddr value))
                (action (cadddr (cdr value)))
                (inhibit-read-only t))
            (with-current-buffer buffer
              (save-excursion
                (set-marker-insertion-type marker nil)
                (goto-char start)
                (when (> length 0)
                  (delete-char length nil))
                (when (and text (> (length text) 0))
                  (if (not action) (insert (propertize text 'read-only t))
                    (insert-text-button
                     text
                     'action 'sturgeon-ui--cursor-action
                     'sturgeon-sink sink
                     'sturgeon-marker marker
                     'read-only t
                     )))
                  (set-marker-insertion-type marker t)
                  ))))
         (t (sturgeon-cancel value)))))))

(defun sturgeon-ui-handler (value)
  (message "%S" value)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let ((buffer (get-buffer-create (cadr value)))
                 (sink (caddr value)))
             (app-any
              sink
              (cons 'sink (sturgeon-ui--make-cursor buffer (point-min) sink)))))
          (t (sturgeon-cancel value)))))

(defun sturgeon-ui-connect (process &rest args)
  (lexical-let ((buffer (current-buffer)) (marker (point-marker)))
    (sturgeon-query process
      (list
       'connect-ui
       (lambda-once (kind value)
         (if (not (eq kind 'feed))
             (progn
               (sturgeon-cancel value)
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char marker)
                   (insert "Connection closed.\n"))))
           (app-any
            sink
            (cons 'sink (sturgeon-ui--make-cursor buffer (marker-position marker) sink)))))
       args))))

;; Done

(provide 'sturgeon)

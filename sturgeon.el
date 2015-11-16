(defcustom sturgeon-debug nil
  "Dump I/O to message buffer")

(defun sturgeon--debug (prefix content)
  (when sturgeon-debug
    (message "%s %S" prefix content)))

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
  (sturgeon--debug "cancelling" sexp)
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
     ((and (eq cmd 'greetings) (eq 1 (car-safe payload)))
      (with-demoted-errors "greetings %S"
        (let ((cogreetings (process-get process 'sturgeon-cogreetings)))
          (if (not cogreetings)
              (sturgeon--cancel-low process answer)
            (process-put process 'sturgeon-cogreetings nil)
            (funcall cogreetings (sturgeon--higher process (cdr payload)))))))
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

(defun sturgeon-start (process &rest rest)
  (process-put process 'sturgeon-lines nil)
  (process-put process 'sturgeon-table (cons 0 (make-hash-table)))
  (process-put process 'sturgeon-cogreetings (plist-get rest :cogreetings))
  (process-put process 'sturgeon-roots
               (cons (make-hash-table)
                     (make-hash-table :weakness 'value)))
  (set-process-filter process #'sturgeon--filter)
  (setq sturgeon--processes (cons process sturgeon--processes))
  (sturgeon--send
   process
   (cons 'greetings (cons 1 (sturgeon--lower process (plist-get rest :greetings)))))
  process)

(defun sturgeon-start-process (name buffer path args &rest rest)
  (let* ((start-file-process
          (if (fboundp 'start-file-process)
              #'start-file-process
            #'start-process))
         (process-connection-type nil)
         (process (apply start-file-process name buffer path args)))
    (apply 'sturgeon-start process rest)))

;; Rich printing facility

(require 'button)

(defvar-local sturgeon--cursors nil)
(defvar-local sturgeon--revision 0)
(defconst sturgeon--active-cursor nil)

;; cursor = [0:buffer 1:sink 2:marker 3:remote-revision 4:changes]
(defun sturgeon--change-cursor (cursor beg end len)
  (let ((point (marker-position (elt cursor 2))))
    (unless (< end point)
      ;; Adjust coordinates
      (setq beg (- beg point))
      (when (< beg 0)
        (setq len (+ len beg))
        (setq beg 0))
      (setq end (- end point))
      ;; Record changes
      (let ((revisions (elt cursor 4)))
        (unless (eq len 0)
          (setq revisions
                (cons (vector sturgeon--revision 'remove beg len) revisions)))
        (unless (eq beg end)
          (setq revisions
                (cons (vector sturgeon--revision 'insert beg (- end beg)) revisions)))
        (aset cursor 4 revisions))
      ;; Commit changes
      (let ((action (list 'substitute
                      (cons (elt cursor 3) sturgeon--revision)
                      (cons beg len)
                      (buffer-substring-no-properties (+ point beg) (+ point end))
                      nil)))
        (message "patch %S" action)
        (app-sink (elt cursor 1) action)))))

(defun sturgeon--change-hook (beg end len)
  (setq sturgeon--revision (1+ sturgeon--revision))
  (dolist (cursor sturgeon--cursors)
    (unless (eq cursor sturgeon--active-cursor)
      (sturgeon--change-cursor cursor beg end len))))

(defun sturgeon--update-revisions (cursor revisions)
  (aset cursor 3 (cdr revisions))
  (let ((pred (lambda (change) (<= (elt change 0) (car revisions)))))
    (aset cursor 4 (delete-if pred (elt cursor 4)))))

(defun sturgeon--remap (s l x)
  (if (< x s) x
    (if (> x (+ s l))
      (- x l)
      s)))

(defun sturgeon--commute-op (cursor k2 s2 l2)
  (dolist (op1 (reverse (elt cursor 4)))
    (let* ((k1 (elt op1 1))
           (s1 (elt op1 2))
           (l1 (elt op1 3)))
      (message "commuting %S %S" k1 k2)
      (cond
       ((and (eq k1 'remove) (eq k2 'remove))
        (let ((e2 (sturgeon--remap s1 l1 (+ s2 l2))))
          (setq s2 (sturgeon--remap s1 l1 s2))
          (setq l2 (- e2 s2))))

       ((and (eq k1 'remove) (eq k2 'insert))
        (unless (< s2 s1)
          (if (< s2 (+ s1 l1))
              (setq l2 0)
            (setq s2 (- s2 l1)))))

       ((and (eq k1 'insert) (eq k2 'remove))
        (if (< s1 s2)
            (setq s2 (+ s2 l1))
          (if (< s1 (+ s2 l2))
              (setq l2 (+ l2 l1)))))

       ((and (eq k1 'insert) (eq k2 'insert))
        (if (< s1 s2)
          (setq s2 (+ s2 l1))))

       (t
         (message "k1:%S k2:%S" k1 k2)
         (assert nil)))))
  (cons s2 l2))

(defun sturgeon-ui--cursor-action (x)
  (let* ((cursor (button-get x 'sturgeon-cursor))
         (sink   (elt cursor 1))
         (marker (elt cursor 2))
         (offset (- (marker-position x) (marker-position marker))))
    (app-sink sink (cons
                    'click
                    (cons (cons (elt cursor 3) sturgeon--revision) offset)))))

(defun sturgeon-ui--make-cursor (buffer point sink)
  (lexical-let ((cursor (vector
                          buffer
                          sink
                          (make-marker)
                          0
                          nil)))
    (set-marker (elt cursor 2) point buffer)
    (set-marker-insertion-type (elt cursor 2) t)
    (setq sturgeon--cursors (cons cursor sturgeon--cursors))
    (make-local-variable 'after-change-functions)
    (add-hook 'after-change-functions 'sturgeon--change-hook)

    (lambda-sink (kind value)
      ;; (when (eq kind 'quit) ...)
      (when (eq kind 'feed)
        (cond
         ;; Clear sub regions
         ((eq (car value) 'substitute)
          (let* ((buffer    (elt cursor 0))
                 (marker    (elt cursor 2))
                 (revisions (elt value 1))
                 (positions (elt value 2))
                 (text      (elt value 3))
                 (action    (elt value 4))
                 (start     (+ (marker-position marker) (car positions)))
                 (length    (cdr positions))
                 (inhibit-read-only t)
                 (sturgeon--active-cursor cursor))
            (sturgeon--update-revisions cursor revisions)
            (with-current-buffer buffer
              (save-excursion
                (set-marker-insertion-type marker nil)
                (when (> length 0)
                  (let ((pos (sturgeon--commute-op cursor 'remove start length)))
                   (when (> (cdr pos) 0)
                     (goto-char (car pos))
                     (delete-char (cdr pos) nil))))
                (when (and text (> (length text) 0))
                  (let ((pos (sturgeon--commute-op cursor 'insert start (length text))))
                   (when (> (cdr pos) 0)
                     ;;(setq text (propertize text 'read-only t))
                     (goto-char (car pos))
                     (if (not action) (insert text)
                       (insert-text-button
                        text
                        'action 'sturgeon-ui--cursor-action
                        'sturgeon-cursor cursor)))))
                (set-marker-insertion-type marker t)))))
         (t (sturgeon-cancel value)))))))

(defun sturgeon-ui-handler (value)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let ((buffer (get-buffer-create (cadr value)))
                 (sink (caddr value)))
             (app-any
              sink
              (cons 'sink (sturgeon-ui--make-cursor buffer (point-min) sink)))))
          (t (sturgeon-cancel value)))))

(defun sturgeon-ui-greetings (buffer &rest args)
  (lexical-let ((buffer buffer) (marker (point-marker)))
    (cons
     'ui-text
     (cons
      (lambda-sink (kind value)
        (if (not (eq kind 'feed))
            (progn
              (sturgeon-cancel value)
              ;; (with-current-buffer buffer
              ;;   (save-excursion
              ;;     (goto-char marker)
              ;;     (insert "Connection closed.\n")))
              )
          (cond
           ((eq (car-safe value) 'accept)
            (switch-to-buffer buffer)
            (let ((point (marker-position marker))
                  (sink (cdr value)))
              (app-any sink
                       (cons 'sink (sturgeon-ui--make-cursor buffer point sink)))))
           ((eq (car-safe value) 'title)
            (with-current-buffer buffer (rename-buffer (cdr value))))
           (t (sturgeon-cancel value)))))
      args))))

(defun sturgeon-launch (filename)
  (interactive "fProgram path: ")
  (let ((buffer (get-buffer-create filename)))
    (sturgeon-start-process
     filename buffer
     filename nil
     :greetings (sturgeon-ui-greetings buffer nil))
    (switch-to-buffer buffer)))

(defun sturgeon-remote-launch (server)
  (interactive "fServer: ")
  (let ((default-directory server))
    (call-interactively 'sturgeon-launch)))

(defun sturgeon-connect (filename)
  (interactive (list
                (let ((insert-default-directory nil))
                  (read-file-name "Socket path: "
                                  (concat temporary-file-directory "sturgeon."
                                          (int-to-string (user-uid)) "/")))))
  (let ((buffer (get-buffer-create filename)))
    (unless (file-name-absolute-p filename)
      (setq filename
            (concat temporary-file-directory
                    "sturgeon." (int-to-string (user-uid)) "/"
                    filename)))
    (sturgeon-start-process
     filename buffer
     "socat" (list "-" (concat "UNIX-CONNECT:" filename))
     :greetings (sturgeon-ui-greetings buffer nil))
    (switch-to-buffer buffer)))

;; Done

(provide 'sturgeon)

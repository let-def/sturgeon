(defcustom sturgeon-debug nil
  "Dump I/O to message buffer")

(defun sturgeon--debug (prefix content)
  (when sturgeon-debug
    (message "%s %S" prefix content)))

(defvar sturgeon--filter-queue 'idle)

(defun sturgeon--filter-action ()
  ; (message "sturgeon--filter-action")
  (let ((current-queue (nreverse sturgeon--filter-queue)))
    (setq sturgeon--filter-queue 'idle)
    (let ((sturgeon--filter-queue nil)
          (proc nil) (lines nil))
      (accept-process-output)
      (while current-queue
        (dolist (item current-queue)
          (setq proc (car item))
          (setq lines (split-string (cdr item) "\n"))
          (unless (cdr lines)
            (process-put proc 'sturgeon-lines
                         (cons (car lines) (process-get proc 'sturgeon-lines))))
          (when (cdr lines)
            (let ((line (cons (car lines) (process-get proc 'sturgeon-lines))))
              (setcar lines (apply 'concat (reverse line))))
            (let ((lines lines))
              (while (cddr lines) (setq lines (cdr lines)))
              (process-put proc 'sturgeon-lines (cdr lines))
              (setcdr lines nil))
            (with-demoted-errors "reading sturgeon input: %S"
              (dolist (line lines)
                (sturgeon--debug ">" line)
                (sturgeon--handler proc line)))))
        (setq current-queue (nreverse sturgeon--filter-queue))
        (setq sturgeon--filter-queue nil)))))

(defun sturgeon--filter (proc lines)
  (when (eq sturgeon--filter-queue 'idle)
    (setq sturgeon--filter-queue nil)
    (run-at-time 0 nil #'sturgeon--filter-action))
  (setq sturgeon--filter-queue
        (cons (cons proc lines) sturgeon--filter-queue)))

;; Routines for working with negations

(defun sturgeon--lambda-extract-handlers (body)
  (let ((handlers nil) (iter body))
    (while (symbolp (car iter))
      (setq handlers (cons
                      (cons `(eq sturgeon-kind ,(car iter)) (cadr iter))
                      handlers))
      (setq iter (cddr iter)))
    (cons handlers iter)))

(defmacro lambda-once (var &rest body)
  (let ((handlers (sturgeon--lambda-extract-handlers body)))
    (setq body (cdr handlers))
    (setq handlers (car handlers))
    `(cons 'once (lambda (sturgeon-kind ,var)
                   (cond
                     ,@handlers
                     ((eq sturgeon-kind 'feed)
                      ,@body)
                     (t
                      (unless (eq sturgeon-kind 'quit)
                        (message "sturgeon: unhandled message %S %S" sturgeon-kind ,var))
                      (sturgeon-cancel ,var)))))))

(defmacro lambda-sink (var &rest body)
  (let ((handlers (sturgeon--lambda-extract-handlers body)))
    (setq body (cdr handlers))
    (setq handlers (car handlers))
  `(cons 'sink (lambda (sturgeon-kind sturgeon-values)
                 (cond
                   ,@handlers
                   ((member sturgeon-kind '(feed batch))
                    (when (eq sturgeon-kind 'feed)
                      (setq sturgeon-values (list sturgeon-values)))
                    (dolist (,var sturgeon-values)
                      (with-demoted-errors "sturgeon: sink application %S"
                       ,@body)))
                   (t
                    (unless (eq sturgeon-kind 'quit)
                      (message "sturgeon: unhandled message %S %S" sturgeon-kind sturgeon-values))
                    (sturgeon-cancel sturgeon-values)))))))

(defmacro lambda-batch (var &rest body)
  (let ((handlers (sturgeon--lambda-extract-handlers body)))
    (setq body (cdr handlers))
    (setq handlers (car handlers))
  `(cons 'sink (lambda (sturgeon-kind ,var)
                 (cond
                   ,@handlers
                   ((member sturgeon-kind '(feed batch))
                    (when (eq sturgeon-kind 'feed)
                      (setq ,var (list ,var)))
                    ,@body)
                   (t
                    (unless (eq sturgeon-kind 'quit)
                      (message "sturgeon: unhandled message %S %S" sturgeon-kind sturgeon-values))
                    (sturgeon-cancel sturgeon-values)))))))

(put 'lambda-once 'lisp-indent-function 'defun)
(put 'lambda-sink 'lisp-indent-function 'defun)
(put 'lambda-batch 'lisp-indent-function 'defun)

(defun sturgeon--app-p (sexp)
  (and (consp sexp)
       (functionp (cdr sexp))
       (member (car sexp) '(once sink))))

(defun app-once (f arg)
  (assert (sturgeon--app-p f))
  (assert (eq (car f) 'once))
  (funcall (cdr f) 'feed arg))

(defun app-any (f arg)
  (assert (sturgeon--app-p f))
  (funcall (cdr f) 'feed arg))

(defun app-sink (f arg)
  (assert (sturgeon--app-p f))
  (assert (eq (car f) 'sink))
  (funcall (cdr f) 'feed arg))

(defun app-batch (f arg)
  (assert (sturgeon--app-p f))
  (assert (eq (car f) 'sink))
  (funcall (cdr f) 'batch arg))

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
  (setq sturgeon--root-collection nil)
  (let ((sturgeon--root-collection t))
    (setq sturgeon--processes
          (delete-if (lambda (process)
                       (member (process-status process)
                               '(exit signal closed failed nil)))
                     sturgeon--processes))
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
         addrs)))))

(defun sturgeon--gc-hook ()
  (unless sturgeon--root-collection
    (setq sturgeon--root-collection 'pending)
    (run-at-time 0 nil #'sturgeon--collect-roots)))

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
      ; (message "REMOVE %S because %S %S" addr (cons (car handler) msg) payload)
      (remhash addr (cdr table)))
    (funcall fn msg payload)))

(defun sturgeon--handler (process answer)
  (setq answer (car (read-from-string answer)))
  (let ((cmd (car-safe answer))
        (payload (cdr-safe answer)))
    (cond
     ((and (eq cmd 'greetings) (equal 1 (car-safe payload)))
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
  (setq command (replace-regexp-in-string "\n" "\\\\n" command))
  (sturgeon--debug "<" command)
  (process-send-string process (concat command "\n")))

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
(defvar-local sturgeon--point-moved nil)
(defvar-local sturgeon--last-point 0)
(defconst sturgeon--active-cursor nil)
(defconst sturgeon--root-collection nil)

;; cursor = [0:buffer 1:sink 2:remote-revision 3:changes 4:latest-remote]
(defun sturgeon--change-cursor (cursor beg end len)
  (setq beg (1- beg))
  (setq end (1- end))
  ;; Record changes
  (let ((changes (elt cursor 3)))
    (unless (equal len 0)
      (setq changes
            (cons (vector sturgeon--revision 'remove beg len) changes)))
    (unless (equal beg end)
      (setq changes
            (cons (vector sturgeon--revision 'insert beg (- end beg)) changes)))
    (aset cursor 3 changes))
  ;; Commit changes
  (let* ((text (encode-coding-string
                (buffer-substring-no-properties (1+ beg) (1+ end))
                'utf-8 t))
         (op (cond
              ((equal (length text) 0)
               (cons 'replace len))
              ((equal len 0)
               (cons 'insert (cons (length text) text)))
              (t
               (cons 'replace (cons len (cons (length text) text))))))
         (action (list 'patch
                       (cons (elt cursor 2) sturgeon--revision)
                       (cons beg op)
                       (cons 'editable nil))))
    (aset cursor 4 sturgeon--revision)
    (app-sink (elt cursor 1) action)))

(defun sturgeon--before-change-hook (beg end)
  (unless sturgeon--active-cursor
    (setq sturgeon--last-point (point))))

(defun sturgeon--after-change-hook (beg end len)
  (setq sturgeon--revision (1+ sturgeon--revision))
  (dolist (cursor sturgeon--cursors)
    (unless (eq cursor sturgeon--active-cursor)
      (sturgeon--change-cursor cursor beg end len))))

(defun sturgeon--update-revisions (cursor revisions)
  (aset cursor 2 (cdr revisions))
  (let ((pred (lambda (change) (<= (elt change 0) (car revisions)))))
    (aset cursor 3 (delete-if pred (elt cursor 3))))
  (when (< (+ 16 (elt cursor 4)) (cdr revisions))
    (aset cursor 4 (cdr revisions))
    (app-sink
     (elt cursor 1)
     `(ack ,(cons (cdr revisions) sturgeon--revision)))))

(defun sturgeon--remap (s l x)
  (if (< x s) x
    (if (> x (+ s l))
      (- x l)
      s)))

(defun sturgeon--commute-op (cursor k2 s2 l2)
  (dolist (op1 (reverse (elt cursor 3)))
    (let* ((k1 (elt op1 1))
           (s1 (elt op1 2))
           (l1 (elt op1 3)))
      ; (message "commuting %S %S" k1 k2)
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
         (message "sturgeon--commute-op: k1:%S k2:%S" k1 k2)
         (assert nil)))))
  (cons s2 l2))

(defun sturgeon-ui--cursor-action (x)
  (let* ((cursor (button-get x 'sturgeon-cursor))
         (sink   (elt cursor 1))
         (rev    (cons (elt cursor 2) sturgeon--revision))
         (offset (1- (marker-position x)))
         (action `(patch ,rev ,offset (propertize . 0) (clicked))))
    (app-sink sink action)))

(defun sturgeon-ui--substitute (cursor offset oldlen text newlen flags)
 (let ((point-begin (point))
       (inhibit-read-only t)
       (sturgeon--active-cursor cursor))
   (save-excursion
     (when (> oldlen 0)
       (let ((pos (sturgeon--commute-op cursor 'remove offset oldlen)))
        (when (> (cdr pos) 0)
          (goto-char (1+ (car pos)))
          (delete-char (cdr pos) nil))))
     (when (and text (> (length text) 0))
       (let ((pos (sturgeon--commute-op cursor 'insert offset (length text))))
        (when (> (cdr pos) 0)
          (unless (member 'raw flags)
            (setq text (decode-coding-string text 'utf-8 t)))
          (unless (member 'editable flags)
            (setq text (propertize text 'read-only t)))
          (when (member 'invisible flags)
            (setq text (propertize text 'invisible t)))
          (goto-char (1+ (car pos)))
          (if (member 'clickable flags)
              (insert-text-button
               text
               'action 'sturgeon-ui--cursor-action
               'sturgeon-cursor cursor)
             (insert text))))))

   ;; Heuristics to place point at natural positions

   ;; (message "%d: removed %d inserted %d %S, point %d moved to %d, last user point was %d, last forced move was %d to %d"
   ;;          (1+ offset) oldlen newlen text point-begin (point)
   ;;          sturgeon--last-point
   ;;          (or (car-safe sturgeon--point-moved) 0) (or (cdr-safe sturgeon--point-moved) 0))

   ;; First check: user removed a character that sturgeon reinserted
   ;;              immediately after
   (when (and (equal (point) point-begin)
              (equal point-begin (1+ offset))
              (equal newlen 1)
              (equal sturgeon--last-point (1+ point-begin)))
     ;; (message "MOVE TO %d" sturgeon--last-point)
     (goto-char sturgeon--last-point))

   ;; Second check: sturgeon reinserted data it had removed at a place
   ;;               where the cursor was and had to be moved.
   (if (not (equal (point) point-begin))
       (setq sturgeon--point-moved (cons point-begin (point)))
     (if (equal point-begin (cdr-safe sturgeon--point-moved))
       (progn
         (goto-char (min (+ 1 offset newlen) (car-safe sturgeon--point-moved)))
         (setq sturgeon--point-moved (cons point-begin (car-safe sturgeon--point-moved))))
       ))
   ))

(defun sturgeon-ui--apply-patch (cursor value)
  (let* ((buffer    (elt cursor 0))
         (revisions (elt value 1))
         (offset    (elt value 2))
         (operation (elt value 3))
         (kind      (car operation))
         (flags     (elt value 4)))
    (sturgeon--update-revisions cursor revisions)
    (with-current-buffer buffer
      (cond
        ((eq kind 'propertize)
         nil) ;; TODO
        ((eq kind 'remove)
         (sturgeon-ui--substitute cursor offset (cdr operation) "" 0 flags))
        ((eq kind 'insert)
         (sturgeon-ui--substitute cursor offset 0
                                  (cadr operation) (cddr operation) flags))
        ((eq kind 'replace)
         (sturgeon-ui--substitute cursor offset (cadr operation)
                                  (caddr operation) (cdddr operation) flags))
      ))))

(defun sturgeon-ui--make-cursor (buffer point sink)
  (lexical-let ((cursor (vector buffer sink 0 nil 0)))
    (with-current-buffer buffer
      (setq sturgeon--cursors (cons cursor sturgeon--cursors))
      (make-local-variable 'before-change-functions)
      (add-hook 'before-change-functions 'sturgeon--before-change-hook)
      (make-local-variable 'after-change-functions)
      (add-hook 'after-change-functions 'sturgeon--after-change-hook)
      (lambda-sink value
        (cond
         ((eq (car value) 'ack)
          (let* ((revisions (cadr value)))
            (sturgeon--update-revisions cursor revisions)))
         ((eq (car value) 'patch)
          (sturgeon-ui--apply-patch cursor value)))))))

(defun sturgeon-ui-handler (value &optional buffer point)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let* ((name (cadr value))
                  (buffer (if (or (not buffer) sturgeon--cursors)
                              (get-buffer-create (generate-new-buffer-name name))
                            (with-current-buffer buffer
                              (rename-buffer name t)
                              buffer)))
                  (point (if point point
                           (with-current-buffer buffer (point-min))))
                  (sink (caddr value)))
             (app-any
              sink (cons 'sink (sturgeon-ui--make-cursor buffer point sink)))))
          (t (sturgeon-cancel value)))))

(defun sturgeon-ui-cogreetings (buffer)
  (lexical-let* ((buffer buffer)
                 (marker (with-current-buffer buffer (point-marker))))
    (lambda (value)
      (cond
       ((eq (car-safe value) 'buffer-shell)
        (app-once (cadr value)
                  (lambda-sink value
                    (let ((point (when buffer (marker-position marker))))
                      (sturgeon-ui-handler value buffer point)))))
       (t (sturgeon-cancel value)))
      )))

(defun sturgeon-launch (filename)
  (interactive "fProgram path: ")
  (let ((buffer (get-buffer-create filename)))
    (sturgeon-start-process
     filename buffer
     filename nil
     :cogreetings (sturgeon-ui-cogreetings buffer))
    (switch-to-buffer buffer)))

(defun sturgeon-connect (name)
  (interactive (list (completing-read
                      "Socket: "
                      (with-demoted-errors "Cannot execute 'sturgeon-connector' command, check your setup. (%S)"
                       (process-lines "sturgeon-connector" "list")))))
  (let ((buffer (get-buffer-create name)))
    (if (and (boundp 'sturgeon--remote) sturgeon--remote)
        (sturgeon-start-process
          name buffer
          "sturgeon-connector" (list "pipe" name)
          :cogreetings (sturgeon-ui-cogreetings buffer))
      (let ((path (or (car-safe (process-lines "sturgeon-connector" "which" name)) name)))
         (sturgeon-start (make-network-process
                          :name name :buffer buffer :family 'local :service path)
                         :cogreetings (sturgeon-ui-cogreetings buffer))))
      (switch-to-buffer buffer)))

(defun sturgeon-remote-launch (server)
  (interactive "fServer: ")
  (let ((default-directory server)
        (sturgeon--remote t))
    (call-interactively 'sturgeon-launch)))

(defun sturgeon-remote-connect (server)
  (interactive "fServer: ")
  (let ((default-directory server)
        (sturgeon--remote t))
    (call-interactively 'sturgeon-connect)))

;; Done

(provide 'sturgeon)
(require 'serge)
(require 'button)

(defun serge-hyperprint--cursor-action (x)
  (let ((action (button-get x 'serge-sink)))
    (if (eq (car action) 'sink)
        (app-sink action (cons 'click (marker-position x)))
      (app-once action t)
      (delete-overlay x))))

(defun serge-hyperprint--make-cursor (buffer sink)
  (lexical-let ((buffer buffer) (sink sink))
    (lambda-sink (kind value)
      ;; (when (eq kind 'quit) ...)
      (when (eq kind 'feed)
        (cond
         ;; Clear sub regions
         ((eq (car value) 'substitute)
          (let ((start  (1+ (cadr value)))
                (length (caddr value))
                (text   (cadddr value))
                (action (cadddr (cdr value))))
            (with-current-buffer buffer
              (save-excursion
                (goto-char start)
                (when (> length 0)
                  (delete-char length nil))
                (if (not action) (insert text)
                  (insert-text-button
                   text
                   'action 'serge-hyperprint--cursor-action
                   'serge-sink sink))))))
         (t (serge-cancel value)))))))

(defun serge-hyperprint-handler (value)
  (message "%S" value)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let ((buffer (get-buffer-create (cadr value)))
                 (sink (caddr value)))
             (app-any sink
                      (cons 'sink (serge-hyperprint--make-cursor buffer sink)))))
          (t (serge-cancel value)))))

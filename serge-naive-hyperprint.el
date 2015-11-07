(require 'serge)
(require 'button)

(defun serge-hyperprint--cursor-action (x)
  (let ((action (button-get x 'serge-action)))
    (if (eq (car action) 'sink)
        (app-sink action t)
      (app-once action t)
      (delete-overlay x))))

(defun serge-hyperprint--make-cursor (buffer)
  (lexical-let ((buffer buffer))
    (lambda-sink (kind value)
      ;; (when (eq kind 'quit) ...)
      (when (eq kind 'feed)
        (cond
         ;; Clear sub regions
         ((eq (car value) 'substitute)
          (let ((start  (cadr value))
                (length (caddr value))
                (text   (cadddr value))
                (action (cadddr (cdr value))))
            (with-current-buffer buffer
              (save-excursion
                (goto-char start)
                (if (not action) (insert text)
                  (insert-text-button text))
                (delete-char length nil)))))
         (t (serge-cancel value)))))))

(defun serge-hyperprint-handler (value)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let ((buffer (get-buffer-create (cadr value)))
                 (cursor (caddr value)))
             (assert (serge--app-p cursor))
             (app-any cursor (serge-hyperprint--make-cursor buffer)))
          (t (serge-cancel value))))))

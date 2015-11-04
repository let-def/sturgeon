(require 'serge)
(require 'button)

(defun serge-hyperprint--clear-markers (markers)
  (when (car markers)
    (set-marker (car markers) nil)
    (set-marker (cadr markers) nil)
    (when (caddr markers) (app-quit (caddr markers)))
    (setcar markers nil)
    (setcdr markers nil)))

(defun serge-hyperprint--cursor-action (x)
  (let ((action (button-get x 'serge-action)))
    (if (eq (car action) 'sink)
        (app-sink action t)
      (app-once action t)
      (delete-overlay x))))

(defconst serge--invisible-text
  (let ((text " "))
    (add-text-properties 0 1 '(invisible t) text)
    text))

(defun serge-hyperprint--make-cursor (buffer point cursor)
  (assert (serge--app-p cursor))
  (lexical-let ((buffer buffer)
                (marker (with-current-buffer buffer
                          (save-excursion
                            (goto-char point)
                            (let ((marker1 (point-marker))
                                  (killswitch (when (eq (car cursor) 'sink) cursor)))
                              (insert serge--invisible-text)
                              (list marker1 (point-marker) killswitch))))))
    (app-any cursor
             (lambda-sink (kind value)
               (when (eq kind 'quit)
                 (serge-hyperprint--clear-markers marker))
               (when (eq kind 'feed)
                 (cond
                  ;; Super region cleared
                  ((equal (car marker) (cadr marker))
                   (serge-hyperprint--clear-markers marker)
                   (serge-cancel value))

                  ;; Clear sub regions
                  ((eq value 'clear)
                   (with-current-buffer buffer
                     (save-excursion
                       (delete-region (car marker) (1- (cadr marker))))))

                  ;; Insert text
                  ((eq (car-safe value) 'text)
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char (1- (cadr marker)))
                       (insert (cadr value)))))

                  ;; Create sub region
                  ((eq (car-safe value) 'sub)
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char (1- (cadr marker)))
                       (let ((action (plist-get value :action))
                             (point0 (point)))
                         (when action (insert serge--invisible-text))
                         (serge-hyperprint--make-cursor buffer (point) (cadr value))
                         (make-button point0 (1- (cadr marker))
                                      'action #'serge-hyperprint--cursor-action
                                      'serge-action action))
                       (goto-char (1- (cadr marker))))))

                  (t (serge-cancel value))))))))

(defun serge-hyperprint-handler (value)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let ((buffer (get-buffer-create (cadr value)))
                 (cursor (caddr value)))
             (serge-hyperprint--make-cursor buffer 0 cursor)))
          (t (serge-cancel value)))))

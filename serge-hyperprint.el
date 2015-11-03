(require 'serge)
(require 'button)

(defun serge-hyperprint--clear-markers (markers)
  (when (car markers)
    (set-marker (car markers) nil)
    (set-marker (cdr markers) nil)
    (setcar markers nil)
    (setcdr markers nil)))

(defun serge-hyperprint--cursor-action (x)
  (funcall (cdr (button-get x 'serge-action))
           'feed t))

(defconst serge--invisible-text
  (let ((text " "))
    (add-text-properties 0 1 '(invisible t) text)
    text))

(defun serge-hyperprint--cursor (buffer point)
  (lexical-let ((buffer buffer)
                (marker (with-current-buffer buffer
                          (save-excursion
                            (goto-char point)
                            (let ((marker1 (point-marker)))
                              (insert serge--invisible-text)
                              (cons marker1 (point-marker)))))))
    (cons
     'sink
     (lambda (kind value)
       (when (member kind '(abort close))
         (serge-hyperprint--clear-markers marker))
       (when (eq kind 'feed)
         (cond
          ;; Super region cleared
          ((equal (car marker) (cdr marker))
           (serge-hyperprint--clear-markers marker)
           (serge-cancel value))

          ;; Clear sub regions
          ((eq value 'clear)
           (with-current-buffer buffer
             (save-excursion
               (delete-region (car marker) (1- (cdr marker))))))

          ;; Insert text
          ((eq (car-safe value) 'text)
           (with-current-buffer buffer
             (save-excursion
               (goto-char (1- (cdr marker)))
               (insert (cadr value)))))

          ;; Create sub region
          ((eq (car-safe value) 'sub)
           (with-current-buffer buffer
             (save-excursion
               (goto-char (1- (cdr marker)))
               (let ((action (plist-get value :action))
                     (point0 (point)))
                 (when action (insert serge--invisible-text))
                 (ignore-errors
                   (funcall (cdr (cadr value))
                            'feed (serge-hyperprint--cursor buffer (point))))
                 (when action
                   (make-button point0 (1- (cdr marker))
                                'action #'serge-hyperprint--cursor-action
                                'serge-action action))
                 (goto-char (1- (cdr marker)))))))

          (t (serge-cancel value))))))))

(defun serge-hyperprint-handler (value)
  (let ((cmd (car-safe value)))
    (cond ((eq cmd 'create-buffer)
           (let ((buffer (get-buffer-create (cadr value)))
                 (thunk (caddr value)))
             (funcall (cdr thunk)
                      'feed (serge-hyperprint--cursor buffer 0))))
          (t (serge-cancel value)))))

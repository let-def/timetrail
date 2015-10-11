(define-minor-mode timetrail-mode
  "Record work on this buffer in timetrail."
  :lighter " timetrail")

(defvar-local timetrail--buffer-dirty nil
  "Has buffer been modified since last check.")

(defcustom timetrail-always t
  "If `t', store activity of all buffers visiting a file. If `nil', only those having timetrail minor mode enabled."
  :group 'timetrail :type 'boolean)

(defun timetrail--track-p ()
  "Should this buffer be stored in timetrail?"
  (and (or timetrail-always timetrail-mode)
       timetrail--buffer-dirty (buffer-file-name)))

(defun timetrail--log (args)
  "Call timetrail program"
  (when args (apply 'call-process "timetrail-log" nil nil nil args)))

(defun timetrail--flush ()
  "Store this buffer in timetrail"
  (when (timetrail--track-p)
    (setq timetrail--buffer-dirty nil)
    (timetrail--log (list (buffer-file-name)))))

(defun timetrail--flush-all ()
  "Send all pending changes to timetrail"
  (let ((buffers nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (timetrail--track-p)
          (setq buffers (cons (buffer-file-name) buffers))
          (setq timetrail--buffer-dirty nil))))
    (timetrail--log buffers)))

(defun timetrail--on-change (start end length)
  "Remember buffer has changed"
  (setq timetrail--buffer-dirty t))

(add-hook 'after-save-hook 'timetrail--flush)
(add-hook 'kill-buffer-hook 'timetrail--flush)
(add-hook 'after-change-functions 'timetrail--on-change)
(run-at-time nil 300 'timetrail--flush-all)

(provide 'timetrail)

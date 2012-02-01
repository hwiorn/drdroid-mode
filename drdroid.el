(defvar selected-device "")

(defun adb-command (command)
  (shell-command-to-string (format "adb \"%s\"" command)))
(defun drdroid-select-device ()
  (let (found t)
       (device-list ())
    (with-temp-buffer
      (insert (adb-command "devices"))
      (goto-char (point-min))
      (while found
	(forward-line)
	(setq found (search-forward "\t" nil t))
	(when found
	  (setq (append device-list (substring (beginning-of-line) (point-min)))))))
    device-list))

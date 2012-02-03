
(eval-when-compile (require 'cl))

(defvar drdroid-selected-device ())
(defvar drdroid-device-list ())
(defvar drdroid-sdk-path ())

;; Android ADB command
(defun adb-devices ()
  (shell-command-to-string "adb devices"))

(defun adb-command (device-id command)
  (shell-command-to-string (format "adb -s \"%s\" %s" device-id command)))

(defun adb-shell (device-id command)
  (shell-command-to-string (format "adb -s \"%s\" shell \"%s\"" device-id command)))

;; Dr.Droid Functions

;; device 목록을 갱신
;; TODO: avd 목록도 같이 가지고 있는 것도 괜찮을 듯
(defun drdroid-get-device-list ()
  (let ((found t)
	(device-list ()))
    (with-temp-buffer
      (insert (adb-devices))
      (goto-char (point-min))
      (while found
	(forward-line)
	(setq found (search-forward "\t" nil t))
	(when found
	  (let* ((founded-point (progn (backward-char)
				       (point)))
		 (beg-point (progn (beginning-of-line)
				   (point)))
		 (device-id (buffer-substring beg-point founded-point)))
	    (setq device-list (append device-list (list device-id)))))))
    (setq drdroid-device-list device-list)
    (when (equal (safe-length device-list) 1)
      (setq drdroid-selected-device (car device-list)))))

;; 선택한 device가 갱신된 device-list에 존재하는지 검사
;; 없으면 선택 device를 초기화
(defun drdroid-check-valid-device-id ()
  (drdroid-get-device-list)
  (when (null (member drdroid-selected-device drdroid-device-list))
    (setq drdroid-selected-device ())))

;; device 를 하나 선택
(defun drdroid-select-device ()
  (interactive)
  (drdroid-check-valid-device-id)

  (if drdroid-device-list
      (let ((init-value (if (member drdroid-selected-device
				    drdroid-device-list)
			    drdroid-selected-device
			  (car drdroid-device-list))))
	(completing-read "select device : "
			 drdroid-device-list
			 nil
			 0
			 init-value
			 'drdroid-selected-device)
	(message "device [%s] selected" drdroid-selected-device))
    (message "device(s) not founded")))

;; 현재 선택된 것이 없을 경우, 다시 선택하도록 하거나 그대로 둠.
(defun drdroid-check-device-or-re-select ()
  (drdroid-check-valid-device-id)
  (when (null drdroid-selected-device)
    (drdroid-select-device))
  drdroid-selected-device)

;; 태그
(defun drdroid-xml-get-attribute (attribute)
  (let ((beg-attr (format "%s=\"" attribute))
	(end-attr "\"")
	(beg-point 0)
	(end-point 0))
    (search-forward beg-attr nil t)
    (setq beg-point (point))
    (search-forward end-attr nil t)
    (backward-char (length end-attr))
    (setq end-point (point))
    (buffer-substring beg-point end-point)))

;; android package 목록을 갱신
(defun drdroid-get-package-list ()
  (when (drdroid-check-device-or-re-select)
    (let ((found t)
	  (package-list ()))
      (with-temp-buffer
	(insert (adb-shell drdroid-selected-device "cat /data/system/packages.xml"))
	(goto-char (point-min))
	(while found
	  (setq found (search-forward "<package " nil t))
	  (when found
	    (let* ((package-name (drdroid-xml-get-attribute "name"))
		   (package-apk (drdroid-xml-get-attribute "codePath"))
		   (package-version (drdroid-xml-get-attribute "version")))
	      (setq package-list (append package-list
					 ;; (list (format "%s(v%s) - %s"
					 ;; 	       package-name
					 ;; 	       package-version
					 ;; 	       package-apk))
					 (list package-name)
					 ))))))
      package-list)))

;; android avd 목록을 갱신
(defun drdroid-get-avd-list ()
  (when (drdroid-check-device-or-re-select)
    (let ((found t)
	  (avd-list ()))
      (with-temp-buffer
	(insert (shell-command-to-string "android list avd"))
	(goto-char (point-min))
	(forward-line)
	(while found
	  (setq found (search-forward "Name: " nil t))
	  (when found
	    (let* ((found-point (point))
		   (avd-name (progn
			       (end-of-line)
			       (buffer-substring found-point (point))))
		   (target-found (search-forward "Target: " nil t))
		   (avd-target (progn
				 (end-of-line)
				 (if target-found
				     (buffer-substring target-found (point))
				   ""))))
	      (setq avd-list (append avd-list
				     (list (format "%s(%s)" avd-name avd-target))
				     ))))))
      avd-list)))


;; package 선택 목록을 만들고 선택하게 되면 지운다
(defun drdroid-uninstall-app (&rest package)
  (interactive)
  (when (drdroid-check-device-or-re-select)  
    (let* ((package-list (drdroid-get-package-list))
	   (package-selected ())
	   (init-package (if package
			     package
			   (if (null package-list)
			       ""
			     (car package-list)))))
      (when package-list
	(completing-read "select package : "
			 package-list
			 nil
			 0
			 init-package
			 'package-selected)
	(when (and (listp package-selected)
		   (y-or-n-p (format "do you really want to uninstall this package[%s] " (car package-selected))))
	  (message (adb-command drdroid-selected-device (format "uninstall \"%s\"" (car package-selected))))) 
	))))

(defun drdroid-manifest-directory ()
  "d:/dev/workspace/android_msfa/"
  )
(defun drdroid-get-drawable-list ()
  ;;TODO:get file-list
  )
(defun drdroid-get-string-list ()
  (let ((found t)
	(string-list ()))
    (with-temp-buffer
      (insert-file (concat (drdroid-manifest-directory) "res/values/strings.xml" ))
      (goto-char (point-min))
      (forward-line)
      (while found
	(setq found (search-forward "<string " nil t))
	(when found
	  (let* ((found-point (point))
		 (string-name (drdroid-xml-get-attribute "name")))
	    (setq string-list (append string-list
				      (list string-name)
				      ))))))
    string-list))

(defun drdroid-directory-files-recurs (dir &optional include-regexp)
  "Get all the files in DIR, and any subdirectories of DIR, whose
names match INCLUDE-REGEXP."
  (let (files)
    (loop for file in (directory-files dir) do
	  (if (not (equal (substring file 0 1) "."))
	      (let ((file (concat dir "/" file)))
		(if (file-directory-p file)
		    (setq files (append files (drdroid-directory-files-recurs file include-regexp)))
		  (if (or (not include-regexp)
			  (string-match include-regexp file))
		      (setq files (append files (list file))))))))
    files))

(defmacro define-drdroid-get-res-list (res-type)
  `(defun ,(intern (concat "drdroid-get-" res-type "-list")) ()
     (let ((list ()))
       (dolist (directory (directory-files (concat (drdroid-manifest-directory) "res/")))
	 (let ((only-file-name
		(file-name-sans-extension (file-name-nondirectory directory)))
	       (dir-path (concat (drdroid-manifest-directory) "res/" directory)))
	   (when (and
		  (not (equal only-file-name "."))
		  (file-directory-p dir-path)
		  (string-match (concat "^" ,res-type ".*") only-file-name))
	     (setq list (append list
				(mapcar (lambda (x)
					  (file-name-sans-extension (file-name-nondirectory x)))
					(drdroid-directory-files-recurs dir-path))))
	     )))
       (delete-dups list))))
(define-drdroid-get-res-list "drawable")
(define-drdroid-get-res-list "layout")
(define-drdroid-get-res-list "anim")
(define-drdroid-get-res-list "color")

(defun drdroid-get-array-list ())
(defun drdroid-get-total-color-list ()
  (let ((found t)
	(color-list ()))
    (with-temp-buffer
      (insert-file (concat (drdroid-manifest-directory) "res/values/colors.xml" ))
      (goto-char (point-min))
      (forward-line)
      (while found
	(setq found (search-forward "<color " nil t))
	(when found
	  (let* ((found-point (point))
		 (color-name (drdroid-xml-get-attribute "name")))
	    (setq color-list (append color-list
				     (list color-name)
				     ))))))
    (append color-list
	    (drdroid-get-color-list))
    ))  


(defun drdroid-get-style-list ()
  (let ((found t)
	(style-list ()))
    (with-temp-buffer
      (insert-file (concat (drdroid-manifest-directory) "res/values/style.xml" ))
      (goto-char (point-min))
      (forward-line)
      (while found
	(setq found (search-forward "<style " nil t))
	(when found
	  (let* ((found-point (point))
		 (style-name (drdroid-xml-get-attribute "name")))
	    (setq style-list (append style-list
				     (list style-name)
				     ))))))
    style-list))  


;; android-mode support tool
;; 지원해야 할 목록들
;;
;; android-ant 	android-ant-clean
;; android-ant-compile 	android-ant-install
;; android-ant-uninstall 	android-debug-activity
;; android-emulate 	android-install-app
;; android-jdb 	android-logcat
;; android-logcat-find-file 	android-logcat-find-file-mouse
;; android-mode 	android-start-activity
;; android-start-ddms 	android-start-emulator
;; android-uninstall-app

(defvar drdroid-exclusive-processes ())
(defun drdroid-start-exclusive-command (name command &rest args)
  (and (not (find (intern name) drdroid-exclusive-processes))
       (set-process-sentinel (apply 'start-process-shell-command name name command args)
                             (lambda (proc msg)
                               (when (memq (process-status proc) '(exit signal))
                                 (setq drdroid-exclusive-processes
                                       (delete (intern (process-name proc))
                                               drdroid-exclusive-processes)))))
       (setq drdroid-exclusive-processes (cons (intern name)
                                               drdroid-exclusive-processes))))

;;project 관련
;; android tool에 update project 명령은 뭐지?
(defun drdroid-create-project ())
(defun drdroid-create-test-project ())
(defun drdroid-create-lib-project ())
(defun drdroid-create-identity ())

;; tool 관련
;; sdk path를 임의로 지정할 수 있도록 설정
(defun drdroid-set-sdk-path ())

;; droiddraw의 path를 임의로 지정할 수 있도록 설정
(defun drdroid-set-droiddraw-path ())

(defun drdroid-start-ddms ()
  (interactive)
  "Launch Dalvik Debug Monitor Service tool."
  (interactive)
  (unless (drdroid-start-exclusive-command "*android-ddms*" "ddms")
    (message "ddms already running")))

(defun drdroid-start-avd-manager ()
  (interactive)
  "Launch Android Virtual Device Manager tool."
  (interactive)
  (unless (drdroid-start-exclusive-command "*android-avd-manager*" "android avd")
    (message "AVD Manager already running")))

(defun drdroid-start-sdk-manager ()
  (interactive)
  "Launch SDK Manager tool."
  (interactive)
  (unless (drdroid-start-exclusive-command "*android-sdk-manager*" "android sdk")
    (message "SDK Manager already running")))


(defun drdroid-start-android-lint ())

(defun drdroid-start-ninepatch ()
  (interactive)
  "Launch Draw 9 Patch tool."
  (interactive)
  (unless (drdroid-start-exclusive-command "*android-sdk-manager*" "draw9patch")
    (message "Draw9Patch already running")))


;;menu
(defun drdroid-menu)
(defun drdroid-minor-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define drdroid-menu map
      "Dr.Droid Menu"
      '("Dr.Droid - v1.0.0.0"
	"----"
	["ddms" drdroid-start-ddms
	 :help "launch ddms"]
	["AVD Manager" drdroid-start-avd-manager
	 :help "launch avd manager"]
	["SDK Manager" drdroid-start-sdk-manager
	 :help "launch sdk manager"]
	"----"
	))
    ;;TODO: define-key
    map))

;;현재 임시 디렉토리는 temporary-file-directory 변수 참조
;;windows에서 에러날때가 있긴 한가봄


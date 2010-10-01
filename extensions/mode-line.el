;;; $Id: mode-line.el 1.5 Wed, 06 Jan 1999 06:18:45 +0100 dude $

(require 'cl)

(display-time)
(when window-system
  (delete* 'display-time-string global-mode-string)
  (let ((first-dot (string-match "\\." (system-name))))
    (setq jglick-user-name-short (substring (user-login-name) 0 3))
    (setq jglick-host-name
	  (if first-dot (substring (system-name) 0 first-dot) (system-name)))
    (setq jglick-host-name-short (substring jglick-host-name 0 3))
    (setq jglick-u-l-n (user-login-name))
    (setq frame-title-format
	  '("" jglick-u-l-n "@" jglick-host-name "    %b    " display-time-string))
    (setq icon-title-format '("" jglick-host-name-short
			      " " jglick-user-name-short " %b"))))
;; XXX kills XEmacs modeline colors!
(setq default-mode-line-format
      '("" mode-line-modified mode-line-buffer-identification " " global-mode-string
	"  %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]  "
	(line-number-mode "L%l ") (column-number-mode "C%c  ")
	(-3 . "%p") " " (-3 . "%P")))
(set-default 'mode-line-modified '(" %1*%1+ "))
(set-default 'mode-line-buffer-identification '("%10b"))

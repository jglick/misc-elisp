;;; $Id: dired.el 1.4 Wed, 06 Jan 1999 06:18:45 +0100 dude $

;; Useless files.
(when (boundp 'dired-font-lock-keywords)
  (nconc dired-font-lock-keywords
	 '((" +[^ ]+\\(~\\|,v\\|#\\|\\.\\(at\\|aux\\|cp\\|cps\\|fn\\|fns\\|ky\\|log\\|pg\\|toc\\|tp\\|tps\\|vr\\|vrs\\|lo\\.pl\\|diff\\|orig\\|tmp\\|bak\\|prcs_aux\\)\\)$" . font-lock-comment-face)
	   (" +[^ ]+\\.prj$" . font-lock-reference-face))))

;; Allow C-u C-x C-f to find file mentioned at point. Handy.
(setq dired-x-hands-off-my-keys nil)

;; Misc.
(setq dired-guess-shell-gnutar t)

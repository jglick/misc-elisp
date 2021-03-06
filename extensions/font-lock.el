;;; $Id: font-lock.el 1.5 Wed, 06 Jan 1999 06:18:45 +0100 dude $

(require 'cl)
(require 'font-lock)
(load "choose-color" 'noerr)

(when window-system
  (setq font-lock-support-mode '((shell-mode . nil)
				 (dired-mode . nil)
				 (mail-mode . nil)
				 (t . lazy-lock-mode)))
  (setq fast-lock-minimum-size 0)
  (setq fast-lock-cache-directories '("~/.emacs-flc"))
  (setq fast-lock-save-events '(save-buffer kill-buffer kill-emacs))
  (if (= emacs-major-version 20)
      (setq lazy-lock-defer-on-scrolling t)
    (setq lazy-lock-defer-driven t))
  (setq lazy-lock-defer-time .5)
  (setq lazy-lock-minimum-size 5000)
  (setq lazy-lock-stealth-lines 100)
  (setq lazy-lock-stealth-nice .1)
  (setq lazy-lock-stealth-time 10)
  (setq lazy-lock-stealth-verbose t)
  
  (setq font-lock-mode-disable-list nil)
  (add-hook 'cperl-mode-hook 'turn-on-font-lock)
  (add-hook 'dired-mode-hook 'turn-on-font-lock)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
  (add-hook 'html-helper-mode-hook 'turn-on-font-lock)
  (add-hook 'shell-mode-hook 'turn-on-font-lock)
  (add-hook 'makefile-mode-hook 'turn-on-font-lock)
  (add-hook 'mail-mode-hook 'turn-on-font-lock)
  (add-hook 'change-log-mode-hook 'turn-on-font-lock)
  (add-hook 'c-mode-hook 'turn-on-font-lock)
  (add-hook 'java-mode-hook 'turn-on-font-lock)
  (add-hook 'rmail-mode-hook 'turn-on-font-lock)
  (add-hook 'mhtml-mode-hook 'turn-on-font-lock)
  (setq font-lock-auto-fontify t)
  (when (fboundp 'global-font-lock-mode) (global-font-lock-mode t))
  
  (setq font-lock-maximum-decoration t)
  (setq font-lock-maximum-size 50000)
  
  (make-face 'font-lock-emphasized-face)
  (make-face 'font-lock-other-emphasized-face)
  (set-face-foreground 'modeline "yellow")
  (set-face-background 'modeline "black")
  (defvar font-lock-emphasized-face 'font-lock-emphasized-face)
  (defvar font-lock-other-emphasized-face 'font-lock-other-emphasized-face)
  )

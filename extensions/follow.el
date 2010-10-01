;;; $Id: follow.el 1.3 Wed, 06 Jan 1999 06:18:45 +0100 dude $

(if (or (>= emacs-minor-version 34) (>= emacs-major-version 20))
    (progn
      (load "follow" 'noerr)
      (setq follow-auto t)
      (setq follow-mode-line-text " Flw")))

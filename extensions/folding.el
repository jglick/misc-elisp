;;; $Id: folding.el 1.1 Fri, 07 Feb 1997 10:37:06 +0100 jesse $

;; folding.el; foldingo.el is newer and cooler but intolerably slow!

(autoload 'folding-mode "folding"
  "Minor mode that simulates a folding editor" t)
(defun folding-mode-find-file-hook ()
  "One of the hooks called whenever a `find-file' is successful."
  (and (assq 'folded-file (buffer-local-variables))
       folded-file
       (folding-mode 1)
       (kill-local-variable 'folded-file)))
(or (memq 'folding-mode-find-file-hook find-file-hooks)
    (setq find-file-hooks (append find-file-hooks '(folding-mode-find-file-hook))))

;;; $Id: misc-funcs.el 1.13.1.2 Sat, 03 Oct 1998 02:21:07 +0200 dude $

(defun collapse nil
  "Collapse lines."
  (interactive)
  (goto-char (point-min))
  (while (< (point) (point-max))
    (delete-blank-lines)
    (forward-line))
  (goto-char (point-min)))

(defun tailf (fname)
  "Tail -n 1000 -f a file and view it."
  (interactive "fTail file: ")
  (let ((buf (generate-new-buffer (concat "*Tailing " fname "*"))))
    (start-process (concat "Tail " fname)
		   buf
		   "tail" "-n" "1000" "-f"
		   (expand-file-name fname))
    (switch-to-buffer buf)
    (goto-char (point-max))
    (view-buffer buf)))

(defun string-regex-subst-all (string from to)
  "Replace all occurrences of FROM (regexp) in STRING with TO and return the result.
STRING itself is not changed. TO may contain e.g. \"\\1\" backreferences."
;; XXX I think ELib does this too.
  (do ((idx 0))
      ((not (string-match from string idx)) string)
    (let ((oldlen (length string)))
      (setq string (replace-match to nil nil string))
      (setq idx (+ (match-end 0) (length string) (- oldlen))))))

(provide 'misc-funcs)

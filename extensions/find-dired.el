;;; $Id: find-dired.el 1.8 Mon, 08 Jun 1998 18:49:11 +0200 dude $

(require 'misc-funcs)

(add-hook 'dired-mode-hook
	  (lambda nil
	    "Clean up find-dired's troubles."
	    (if (equal (buffer-name (current-buffer)) "*Find*")
		(let ((stripped (file-name-nondirectory
				 (directory-file-name default-directory))))
		  (rename-buffer (concat "*Find "
					 (if (equal "" stripped) "/"
					   stripped)
					 "*")
				 t)
		  (setq dired-directory (concat dired-directory "./"))
		  (setq dired-buffers
			(delq (rassq (current-buffer) dired-buffers)
			      dired-buffers))))))

(defvar jglick-find-grep-have-perl nil)
(dolist (dir exec-path)
  (when (and dir
	     (let ((perl (expand-file-name "perl" dir)))
	       (and (file-executable-p perl)
		    (file-regular-p perl))))
    (setq jglick-find-grep-have-perl t)))
(defvar jglick-find-grep-options "-i")
(defun jglick-find-grep-dired (verbose dir rx)
  "Find files in DIR containing a regexp RX and start Dired on output.
The command run (after changing into DIR) is

    find . -type f -exec egrep -q 'ARG' {} \\; -ls

With prefix arg, show matching lines. RX need not be escaped for the shell.
Only text files (acc. to Perl, if available) will be checked."
  (interactive "p\nDFind-grep (directory): \nsFind-grep (egrep regexp): ")
  (find-dired dir
	      (concat "-type f "
		      (if jglick-find-grep-have-perl
			  "-exec perl -e 'exit -B shift' {} \\; "
			"")
		      "-exec egrep "
		      (if (> verbose 1) "-n " "-q ")
		      jglick-find-grep-options " -e '"
		      (string-regex-subst-all rx "'" "'\"'\"'")	; escape ticks
		      "' {} \\; ")))

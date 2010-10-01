;;; $Id: shell.el 1.18.1.5 Sat, 14 Nov 1998 06:31:45 +0100 dude $

;; ToDo:
;; TTY emulation?

(require 'cl)

(autoload 'shell-mode "shell" "Shell mode." t)
(setq comint-password-prompt-regexp "\\(\\(\\('s \\|[Oo]ld \\|[Nn]ew \\|[Ee]nter \\|^\\)[Pp]assword\\|pass phrase\\( again\\)?\\)\\|Enter key\\):\\s *\\'")
(setq comint-input-ring-size 500)

(defun custom-shell (basename prog &optional which hist start dir &rest args)
  "Run a custom program in a shell window.
Arguments:
BASENAME is base of buffer name. (It will be reused.)
PROG is full path to program.
WHICH is prefix argument; start a differently-named shell if set.
HIST is a history file to read from and write to periodically.
START is a file of commands to feed to the process at startup.
DIR is a directory to change to upon startup.
ARGS will be passed on to the process' command line."
  (let* ((name (if (and which (/= which 1))
		   (concat basename (number-to-string which))
		 basename))
	 (buf (or (get-buffer name)
		  (prog1
		      (set-buffer (apply 'make-comint name prog
					 (if (and start (file-exists-p start))
					     start
					   nil)
					 args))
		    (rename-buffer name)
		    (shell-mode)
		    (when hist
		      (setq comint-input-ring-file-name hist)
		      (comint-read-input-ring t)
		      (make-variable-buffer-local 'comint-input-filter-functions)
		      (add-hook 'comint-input-filter-functions
				(lambda (ignore)
				  (if (zerop (random 25))
				      (comint-write-input-ring)))))))))
    (switch-to-buffer buf)
    (if dir (cd dir))))

(defun bash (x) "Run bash." (interactive "p") (custom-shell "shell" "/bin/bash" x "~/.bash_history" "~/.emacs_bash" nil "-i"))
(defun su (x) "Run su." (interactive "p") (custom-shell "su" "/bin/su" x "~root/.bash_history" "~root/.emacs_bash" "~root" "-"))

(push '("/\\.emacs_[a-z]+\\'" . sh-mode) auto-mode-alist)

(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)
(add-hook 'comint-output-filter-functions
	  'comint-strip-ctrl-m)
;(defadvice comint-strip-ctrl-m (around comint-strip-ctrl-m-marker-snafu activate)
;  "Handle nasty marker-does-not-point-anywhere messages after first
;return in a shell buffer."
;  (interactive)
;  (condition-case err
;      ad-do-it
;    (error (let ((datum (cadr err)))
;	     (when (or (not (stringp datum))
;		       (not (string-equal (cadr err) "Marker does not point anywhere")))
;	       (signal (car err) (cdr err)))))))
(add-hook 'shell-mode-hook
	  (lambda nil (setq mode-line-process '(" " default-directory " %s"))))
(add-hook 'comint-mode-hook
	  (lambda nil (setq comint-scroll-show-maximum-output t)))


(autoload 'ssh "ssh" "Ssh client mode." t)

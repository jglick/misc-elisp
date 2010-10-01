;;; $Id: useful-keys.el 1.16 Wed, 06 Jan 1999 06:18:45 +0100 dude $

(require 'cl)

(global-set-key [(shift kp-6)] 'other-window)
(global-set-key [(shift kp-4)] (lambda nil (interactive) (other-window -1)))
(global-set-key [(control kp-right)] 'other-frame)
(global-set-key [(control kp-left)] (lambda nil (interactive) (other-frame -1)))
(global-set-key [kp-enter] 'jglick-switch-to-buffer)
(global-set-key [(meta kp-enter)] 'jglick-change-buffer)
(global-set-key [(shift delete)] (lambda nil (interactive) (kill-buffer nil)))
(global-set-key [(control tab)] 'indent-region)
(global-set-key [insert] 'indent-new-comment-line)
(global-set-key [(meta insert)] 'overwrite-mode)
(global-set-key [(meta kp-insert)] 'overwrite-mode)
;; XXX the default in 20.3:
(global-set-key [(control meta %)] 'query-replace-regexp)
(global-set-key [f1] 'rename-buffer)
(global-set-key [f2] (lambda nil (interactive) (view-buffer (current-buffer))))
(global-set-key [(shift f2)] 'view-file)
(global-set-key [f3] 'jglick-find-grep-dired)
(global-set-key [(shift f3)] 'find-name-dired)
(global-set-key [(control shift f3)] 'find-dired)
(global-set-key [f4] 'revert-buffer)
(global-set-key [f5] 'column-shade-mode)
(global-set-key [f7] 'normal-mode)
(global-set-key [(shift f7)] 'ps-print-buffer-with-faces) ;; heh
(global-set-key [f8] 'follow-mode)
(global-set-key [(shift f8)] 'follow-delete-other-windows-and-split)
(global-set-key [f9] 'font-lock-fontify-buffer)
(global-set-key [f10] 'tailf)
(global-set-key [f11] 'goto-line)
(global-set-key [f12] 'auto-fill-mode)
(global-set-key [(shift down-mouse-3)] 'imenu)

(defun jglick-switch-to-buffer nil
  "As much as possible like `switch-to-buffer', but will never create
new buffers (which I find rather annoying). Will switch immediately to
closest matching buffer name, saving time. If you actually need to
create a new buffer, just use `switch-to-buffer' as usual."
  (interactive)
  (switch-to-buffer (read-buffer "Switch to buffer: " (other-buffer) t)))

(defvar jglick-dumb-buffers nil
  "List of buffer names to ignore when changing buffer.")

(defvar jglick-dumb-buffers-regexp '("^\\*.+\\*$" "^TAGS" "\\.prj$")
  "List of regexps for buffer names to ignore when changing buffer.")

(defun jglick-avail-buffers nil
  "Available buffers suited for display."
  (remove-if-not (lambda (buf)
		   (let ((name (buffer-name buf)))
		     (and (not (get-buffer-window buf t))
			  (not (= ?  (aref name 0)))
			  (not (find name jglick-dumb-buffers :test 'string-equal))
			  (not (some (lambda (rx) (string-match rx name))
				     jglick-dumb-buffers-regexp)))))
		 (buffer-list)))

(defun jglick-change-buffer nil
  "Go to a different buffer. The order may be counterintuitive but
this is the best way I know of."
  (interactive)
  (switch-to-buffer (car (last (jglick-avail-buffers)))))

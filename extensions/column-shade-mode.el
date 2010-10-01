(require 'cl)

;;; TO USE: Load this file; and type `M-x column-shade-mode' (in a
;;; Font-Lock enabled bufer) to toggle on or off.

;; Note: it may be desirable to have separate shade lists for
;; different major modes. In particular, Lisp code tends to use the
;; full granularity pretty frequently, whereas block-structured
;; languages like C, Java and Perl tend to use indentation in fixed
;; four-character blocks, while something like HTML may have a number
;; of different styles. Of course, you may set this following as a
;; local variable in your favorite major-mode hook if you choose:
(defvar column-shade-shades '("alice blue"
			      "lavender blush"
			      "lemon chiffon"
			      "mint cream"
			      "antique white")
"*Colors to use as backgrounds in Column Shade mode. PASTELS!!!")
;; Note that some of these may be invisible on a laptop. Also, they
;; assume a white background for Emacs, which for some reason is not
;; the default on at least some systems (e.g. RHL).

(defvar column-shade-use-vert-bars nil
  "*Use vertical bars to shade. This may be more visually appealing
(or not), but it will also slow down display somewhat.")

;;; -------------------------------------------------------------------

(defvar column-shade-face-cache-counter 0)
(defvar column-shade-face-name-prefix "column-shade-face-")
(defvar column-shade-face-cache nil)
(defun column-shade-lazily-get-face (shade)
  (let ((existing (find shade column-shade-face-cache
			:test 'string-equal
			:key 'car)))
    (if existing
	(cdr existing)
      (let ((newface (intern (concat column-shade-face-name-prefix
				     column-shade-face-cache-counter))))
	(push (cons shade newface) column-shade-face-cache)
	(incf column-shade-face-cache-counter)
	(make-face newface)
	(set-face-background newface shade)
	newface))))

(defconst column-shade-horiz-bars
  '("^\\([ \t]+\\)\\(\\S-\\|$\\)"
    (1
     (column-shade-lazily-get-face
      (nth (mod (letf (((point) (match-end 1)))
 		  (current-column))
 		(length column-shade-shades))
 	   column-shade-shades))))
  "Show horizontal bars, shaded according to length. This is faster.")

(defconst column-shade-vert-bars
  '("[ \t]"
    (0
     (let ((c (match-end 0)))
       (letf (((point) c))
	 (beginning-of-line)
	 (when (not (search-forward-regexp "\\S-" c t))
	   (goto-char c)
	   (column-shade-lazily-get-face
	    (nth (mod (current-column)
		      (length column-shade-shades))
		 column-shade-shades)))))))
  "Show vertical bars, shaded according to position. This is slower.")

(defvar column-shade-mode nil
  "Whether Column Shade mode is on in this buffer.")
(make-variable-buffer-local 'column-shade-mode)

(defun column-shade-mode (arg)
  "Toggle Column-Shade mode on or off. With prefix arg, do the usual."
  (interactive "P")
  (let ((new-csm (if (null arg)
		     (not column-shade-mode)
		   (> (prefix-numeric-value arg) 0))))
    (cond
     ((and (not column-shade-mode) new-csm)
      (setq font-lock-keywords
	    ;; Give it lowest priority.
	    (append font-lock-keywords
		    (list
		     (if column-shade-use-vert-bars
			 column-shade-vert-bars
		       column-shade-horiz-bars))))
      (font-lock-fontify-buffer))
     ((and column-shade-mode (not new-csm))
      (setq font-lock-keywords
	    (remove* column-shade-vert-bars
		     (remove* column-shade-horiz-bars font-lock-keywords :test 'equal)
		     :test 'equal))
      (font-lock-fontify-buffer))
     (t nil))
    (setq column-shade-mode new-csm)))

(pushnew '(column-shade-mode " Shd") minor-mode-alist :test 'equal)

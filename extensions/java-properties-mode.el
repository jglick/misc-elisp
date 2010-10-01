(require 'cl)

(pushnew '("\\.properties\\(\\.[a-zA-Z0-9_]+\\)?\\'" . java-properties-mode)
	 auto-mode-alist
	 :test 'equal)

(defvar java-properties-mode-map
  (let ((map (make-sparse-keymap)))
    ;; no bindings for now
    map)
  "Keymap for `java-properties-mode'.")

(defvar java-properties-mode-hook nil
  "Hooks to run on entry to Java Properties mode.")

;; XXX this should be checked against official syntax!
(defvar java-properties-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Special meaning:
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?! "<" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?. "_" table)
    ;; Meaning to mode, but not as a syntax property:
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?: "." table)
    ;; Meaning to Dynamo special treatment, but not o.w.:
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?^ "." table)
    ;; Reasonable property name constituents:
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?/ "_" table)
    (modify-syntax-entry ?? "_" table)
    ;; No particular meaning:
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?@ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?~ "." table)
    table)
  "Syntax table for `java-properties-mode'.")

(defvar java-properties-mode-abbrev-table nil
  "Abbrev table used in Java Properties mode.")
(define-abbrev-table 'java-properties-mode-abbrev-table nil)

;; XXX para mvmnt funcs would be nice
;; should be added as fifth arg of font-lock-defaults too

;; Cribbed from make-mode.el:
(defface java-properties-space-face
   '((((class color)) (:background  "hotpink"))
     (t (:reverse-video t)))
  "Face to use for highlighting trailing spaces in Java Properties mode."
  :group 'faces)
(defface java-properties-continuation-face
   '((((class color)) (:background  "khaki"))
     (t (:reverse-video t)))
  "Face to use for highlighting continuations in Java Properties mode."
  :group 'faces)
(if (fboundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces 'java-properties-space-face)
    (add-to-list 'facemenu-unlisted-faces 'java-properties-continuation-face))
(defvar java-properties-space-face 'java-properties-space-face
  "Face to use for highlighting trailing spaces in Java Properties mode.")
(defvar java-properties-continuation-face 'java-properties-continuation-face
  "Face to use for highlighting continuations in Java Properties mode.")

;; XXX could also use ...-space-face for things like comments w/
;; non-whitespace before them, and for bare text on a line that cannot
;; be a value

(defvar java-properties-font-lock-keywords
  '(
    ;; Comments (trickier than syntactic fontification):
    ("^\\s-*#.*$" 0 font-lock-comment-face)
    ;; For Dynamo:
    ("^ *\\(\\$\\(class\\|scope\\)\\) *[=:]" 1 font-lock-keyword-face)
    ("^ *\\$class *[=:] *\\([a-zA-Z0-9_.]+\\)$" 1 font-lock-type-face)
    ("^ *\\$scope *[=:] *\\([a-z]+\\)$" 1 font-lock-keyword-face)
    ;; General property setting:
    ("^ *\\(\\(\\sw\\|\\s_\\|[][]\\|\\\\.\\)+\\)[+^]? *[=:]" 1 font-lock-variable-name-face)
    ;; Nasty trailing spaces:
    ("\\S-\\(\\s-+\\)$" 1 java-properties-space-face)
    ;; Continued values:
    ("\\\\\n\\s-*" 0 java-properties-continuation-face)
    ;; Escapes:
    ("\\\\\\([a-tv-z#! \\\\]\\|u[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)"
     0 font-lock-string-face t)
    ;; java.text.MessageFormat & the like:
    ;; XXX much richer options are available!
    ("{[0-9]}" 0 font-lock-reference-face)
    ("\\${\\([^}]+\\)}" 1 font-lock-reference-face)
    )
  "What to fontify in Java Properties mode.")

(defun java-properties-mode ()
  "Major mode for editing Java properties files.
Runs `java-properties-mode-hook'.

This mode has a very simple indentation structure: any line which ends
with a backslash is assumed to be a continuation of a value, so the
following line must be indented with a TAB. All other lines are flush
left. Comments are always flush left.

You may want to do this to load this file on demand:

    (autoload 'java-properties-mode \"java-properties-mode\" \"Major mode for editing Java Properties files.\" t)
    (setq auto-mode-alist (cons '(\"\\\\.properties\\\\'\" . java-properties-mode) auto-mode-alist))

\\{java-properties-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map java-properties-mode-map)
  (set-syntax-table java-properties-mode-syntax-table)
  (setq local-abbrev-table java-properties-mode-abbrev-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'java-properties-mode-indent-line)
  (java-properties-mode-setup-comments)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(java-properties-font-lock-keywords t nil))
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression '((nil "^ *\\(\\(\\sw\\|\\s_\\)+\\)[+^]? *[=:]" 1)))
  (setq major-mode 'java-properties-mode)
  (setq mode-name "Java Properties")
  (run-hooks 'java-properties-mode-hook))

(defun java-properties-mode-setup-comments ()
  "Set up comment-related variables."
  (setq comment-column 0)
  (make-local-variable 'comment-start-skip)
  ;; adapted from lisp-mode:
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)[#!]+ *")
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function (lambda nil 0)))

(defun java-properties-mode-indent-line ()
  "Put a line to the first column, unless the last line ended in a
backslash, in which case this is assumed to be a continuation of a
property value, so make sure line starts with exactly one TAB."
  (interactive)
  (let ((cont-p
	 (save-excursion
	   (beginning-of-line)
	   (eq ?\\ (char-after (- (point) 2))))))
    (if cont-p
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (when (not (looking-at "^\t\\($\\|\\S-\\)"))
	      (delete-region (point) (re-search-forward "\\s-*"))
	      (insert "\t")))
	  (if (bolp) (forward-char 1)))
      (save-excursion
	(beginning-of-line)
	(when (not (looking-at "^\\($\\|\\S-\\)"))
	  (delete-region (point) (re-search-forward "\\s-*")))))))

(provide 'java-properties-mode)

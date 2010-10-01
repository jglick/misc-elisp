;;; $Id: makefile.el 1.6 Mon, 23 Feb 1998 17:33:47 +0100 jesse $

(push '("\\.\\(am\\|make\\)\\'" . makefile-mode) auto-mode-alist)

;; Fontification: complete rewrite.

(eval-after-load
 "make-mode"
 '(setq
   makefile-font-lock-keywords
   (let* ((nest-depth 2)
	  ;; I.e. plain text; OR non-alphabetic var; OR (optional)
	  ;; `$' plus `()' or `{}' bracketed stuff; commas are
	  ;; presently _not_ checked for, though it might be nice
	  ;; in functions; no colons or tabs (sorry about shell
	  ;; funcs with them...); colons OK except at top level
	  (nest-rx
	   (labels ((nest-calc
		     (lvl)
		     (if (zerop lvl) "[^:\t\n$(){}]"
		       (let ((sub (nest-calc (- lvl 1))))
			 (concat
			  "\\([^"
			  (if (= lvl nest-depth) ":" "") ; ecch
			  "\t\n$(){}]\\|\\(\\$[^ ({\t\na-zA-Z0-9_]\\)\\|\\(\\$?\\(\\(("
			  sub
			  "*)\\)\\|\\({"
			  sub
			  "*}\\)\\)\\)\\)")))))
	     (nest-calc nest-depth)))
	  (nest-rxs (concat "\\(" nest-rx "*\\)")))
     `(
       ;; Includes
       ("^ *-?include\\([^a-zA-Z_0-9].*\\)" 1 'bold prepend)
       ;; `.PHONY' targets
       ("^ *\\.PHONY *::? *\\(.*\\)" 1 font-lock-emphasized-face prepend)
       ;; Special-execution commands: `+' and `$(MAKE)'; will show
       ;; multi-line commands
       ("^\t+\\(\\+\\(\\\\\n\\|.\\)*\\)$" 1 font-lock-emphasized-face prepend)
       ("^\t+\\(\\(\\\\\n\\|.\\)*\\$(MAKE)\\(\\\\\n\\|.\\)*\\)$"
	1 font-lock-emphasized-face prepend)
       ;; Targets
       (,(concat "^\\(" nest-rxs "::?\\)\\($\\|[^=]\\)")
	1 font-lock-reference-face prepend)
       ;; Normal vars, possibly with substs: `(' and `{'
       ("\\$[({]\\([a-zA-Z0-9_.-]+\\)[)}:]" 1 font-lock-variable-name-face prepend)
       ;; Functions
       ("\\$[({]\\([a-z-]+\\)[ \t]" 1 font-lock-function-name-face prepend)
       ;; Shell function
       (,(concat "\\$(shell[ \t]+\\(" nest-rxs "\\))") 1 'underline prepend)
       (,(concat "\\${shell[ \t]+\\(" nest-rxs "\\)}") 1 'underline prepend)
       ;; Special vars
       ("\\(^\\|[^\n$]\\)\\$\\([^\n$({a-zA-Z0-9_]\\)"
	2 font-lock-keyword-face prepend)
       ("\\$[({]\\(\\(MAKEFILES\\|VPATH\\|SHELL\\|MAKE\\|MAKELEVEL\\|MFLAGS\\|MAKEFLAGS\\|SUFFIXES\\)\\|\\([@*%<^+?][DF]?\\)\\)[)}]"
	1 font-lock-keyword-face prepend)
       ;; Weird space stuff that will confuse make
       ("^[ \t]+$" 0 makefile-space-face)
       ("^\t+#" 0 makefile-space-face t)
       ("^\\( +\\)\t" 1 makefile-space-face)
       ;; Variable defs
       ;; For some reason, fails on e.g. FOO = bar baz \ [newline] ...
       ("^ *\\(\\(override\\|export\\)[ \t]+\\)?\\([a-zA-Z0-9_.-]+\\)[ \t]*[+:]?="
	3 font-lock-type-face t)
       ("^ *\\(un\\)?export[ \t]+\\([a-zA-Z0-9_.-]+\\)"
	2 font-lock-type-face t)
       ;; Special commands
       ("^ *\\(define\\|endef\\|ifdef\\|ifndef\\|ifeq\\|ifneq\\|else\\|endif\\|include\\|\\(override +define\\)\\|override\\|export\\|unexport\\|vpath\\)\\>"
	1 font-lock-keyword-face t)
       ;; Sequence defs
       ("^ *\\(override +\\)?define +\\([a-zA-Z0-9_.-]+\\)$"
	2 font-lock-type-face t)
       ;; Special targets
       ("^ *\\(\\.[A-Z_-]+\\) *::?" 1 font-lock-keyword-face t)
       ;; `$$' is literal
       ("\\$\\$" 0 font-lock-string-face t)
       ;; Comments
       ("^[^\t\n]*\\(#.*\\)" 1 font-lock-comment-face t)
       ))))

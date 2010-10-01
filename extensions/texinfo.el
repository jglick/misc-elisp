;;; $Id: texinfo.el 1.3 Thu, 08 May 1997 10:37:08 +0200 jesse $

(require 'cl)
(push '("\\.tim\\'" . texinfo-mode) auto-mode-alist)

(add-hook 'texinfo-mode-hook (lambda nil (interactive) 
			       (auto-fill-mode) (setq fill-column 87)))

;; Fontification: a complete rewrite.

(eval-after-load
 "texinfo"
 ;; Increase the depth for more accuracy but less speed. E.g.: depth
 ;; of 2 means that up to three nested commands will be treated OK,
 ;; like @b{a @r{b @i{c} d} e}, but more will break the outer ones.
 '(let* ((nest-depth 2)
	 (nest-rx
	  (labels ((nest-calc
		    (lvl)
		    (if (zerop lvl) "[^@{}]"
		      (concat "\\([^@{}]\\|@\\([^a-zA-Z \t\n]\\|\\([a-zA-Z]+\\({"
			      (nest-calc (- lvl 1))
			      "*}\\)?\\)\\)\\)"))))
	    (nest-calc nest-depth)))
	 (nest-rxs (concat "\\(" nest-rx "*\\)")))
    (setq texinfo-font-lock-keywords
	  ;; Order is important!
	  `(
	    ;; @-commands
	    ("@\\(\\(\\(end \\)?[a-zA-Z]+\\)\\|[^ \t\n]\\)" . font-lock-function-name-face)
	    ("[{}]" . font-lock-function-name-face)
	    ;; Emphasized text
	    (,(concat "@\\(emph\\|strong\\|dfn\\){" nest-rxs "}") 2 'italic prepend)
	    ;; Literal text
	    (,(concat "@\\(math\\|samp\\|file\\|kbd\\|key\\){" nest-rxs "}") 2 font-lock-string-face keep)
	    ;; Keyword-style text
	    (,(concat "@\\(code\\|var\\){" nest-rxs "}") 2 font-lock-keyword-face keep)
	    ;; Table/list entries
	    ("@itemx?[ \t]+\\(.+\\)" 1 font-lock-variable-name-face keep)
	    ("@item[ \t]*$" 0 font-lock-variable-name-face t)
	    ;; Definitions
	    ("^@def[a-z]+[ \t]+\\(.*\\)" 1 font-lock-type-face keep)
	    ;; Special typefaces & footnotes
	    (,(concat "@b{" nest-rxs "}") 1 'bold prepend)
	    (,(concat "@i{" nest-rxs "}") 1 'italic prepend)
	    (,(concat "@\\([wtr]\\|footnote\\){" nest-rxs "}") 2 'underline prepend)
	    ;; Escapes
	    ("@[@{}]" 0 font-lock-string-face t)
	    ;; Node lines
	    ("^@node[ \t]+\\(.*\\)" 1 font-lock-reference-face t)
	    ;; Includes
	    ("^@include[ \t]+\\(.*\\)" 1 'bold-italic t)
	    ;; Index entries
	    ("^@[a-z]+index[ \t]+\\(.*\\)" 1 font-lock-keyword-face prepend)
	    ;; Variables
	    ("^@\\(if\\)?\\(set\\|clear\\)[ \t]+\\([a-zA-Z0-9_.-]+\\)" 3 font-lock-keyword-face t)
	    ("^@set[ \t]+[a-zA-Z0-9_.-]+[ \t]+\\(.+\\)" 1 'bold keep)
	    ("@value{\\([a-zA-Z0-9_.-]+\\)}" 1 font-lock-keyword-face prepend)
	    ;; Cross-references; no nesting permitted on node names!
	    ("@\\(cite\\|ref\\|xref\\|pxref\\|inforef\\){\\([^@{}]*\\)}" 2 font-lock-reference-face prepend)
	    ;; Menus
	    ("^\\*[ \t]*[^\n:]+:\\(:\\|\\([^\n.]+\\.\\)\\)" 0 font-lock-reference-face t)
	    ;; Sectioning commands
	    ("^@\\(chapter\\|section\\|subsection\\|subsubsection\\|top\\|unnumbered\\|unnumberedsec\\|unnumberedsubsec\\|unnumberedsubsubsec\\|appendix\\|appendixsec\\|appendixsubsec\\|appendixsubsubsec\\|majorheading\\|chapheading\\|heading\\|subheading\\|subsubheading\\)[ \t]+\\(.*\\)" 2 'bold keep)
	    ;; Comments
	    ("\\(^\\|[^@]\\)\\(\\(@c\\(omment\\)?\\)\\>.*\\)" 2 font-lock-comment-face t)
	    ))))

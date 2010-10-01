;;; $Id: cperl.el 1.16 Fri, 02 Oct 1998 20:52:50 +0200 dude $

(require 'cl)

(push '("\\.\\([Pp][LlMmh]\\|t\\|ix\\|al\\|cgi\\|bgpl\\|pod\\)\\'" . perl-mode)
      auto-mode-alist)
(push '("\\.xs\\'" . c-mode) auto-mode-alist)

;; Actually should have a perldb.el here. Oh well.
(autoload 'perldb "perldb" "Emacs 19/Perl 5 debugging mode" t)
;; Fix that annoying habit of leaving script name in $ARGV[0], arrgh!
(add-hook 'perldb-mode-hook (lambda nil (interactive) (send-string nil "shift\n")))

(autoload 'perl-mode "cperl-mode" "19-enhanced Perl mode" t)
(autoload 'cperl-mode "cperl-mode" "19-enhanced Perl mode" t)
(defvar perl-indent-level 2)
(defvar perl-continued-statement-offset 2)
(defvar perl-continued-brace-offset 0)
(defvar perl-brace-offset 0)
(defvar perl-brace-imaginary-offset 0)
(defvar perl-label-offset -1)

(setq cperl-electric-linefeed t)
(setq cperl-electric-keywords t)
(setq cperl-hairy t)
;;  (setq cperl-lazy-help-time 0)		; Doesn't work at all. Argh.

(setq cperl-info-page "perl5")
(add-hook
 'cperl-mode-hook
 (lambda nil (interactive)
   (local-set-key "\C-hv" 'describe-variable)
   (local-set-key [f9] (lambda nil (interactive)
			 (font-lock-fontify-buffer)
			 (cperl-find-pods-heres)))))

(add-hook 'cperl-mode-hook (lambda nil (interactive) (setq fill-column 87)))

;; Prevent `indent-new-comment-line' from screwing up e.g. $#foo.
(add-hook 'cperl-mode-hook
	  (lambda nil (interactive) (setq comment-start-skip "#[ \t]+")))

;; Fontification: misc. changes; Boogie/Bugout directive highlighting.

;; Will not take effect on 1st file to be visited in Cperl-Mode. Use
;; M-x normal-mode to fix this one; then it should be fine.
(add-hook				; eval-after-load fails!
 'cperl-mode-hook
 (lambda nil (interactive)
   (labels ((q-delim-rx
	     (d)
	     (let ((dq (regexp-quote d)))
	       (concat "\\<q[qwx]?\\s-*" dq "\\(\\([^" d "\\]\\|\\\\.\\)*\\)" dq)))
	    (nest-delim2-rx
	     (n d1 d2)
	     (if (zerop n)
		 (concat "[^" d2 d1 "\\]") ; This order for "[^][]".
	       (let ((sub (nest-delim2-rx (- n 1) d1 d2))
		     (dq1 (regexp-quote d1))
		     (dq2 (regexp-quote d2)))
		 (concat "\\([^" d2 d1 "\\]\\|\\\\.\\|" dq1 sub "*" dq2 "\\)"))))
	    (q-delim2-rx
	     (n d1 d2)
	     (let ((sub (nest-delim2-rx n d1 d2)) ; Nest: 0=q(abc) 1=q(a (bc) d) ...
		   (dq1 (regexp-quote d1))
		   (dq2 (regexp-quote d2)))
	       (concat "\\<q[qwx]?\\s-*" dq1 "\\(" sub "*\\)" dq2))))
     (setq perl-font-lock-keywords-2
	   `
	   (("\\(^\\|[^$@%&\\]\\)\\<\\(if\\|until\\|while\\|elsif\\|else\\|unless\\|for\\|foreach\\|continue\\|exit\\|die\\|last\\|goto\\|next\\|redo\\|return\\|local\\|exec\\|sub\\|do\\|dump\\|use\\|require\\|package\\|eval\\|my\\|BEGIN\\|END\\)\\>" .
	   2)
	    ("\\(^\\|[^$@%&\\]\\)\\<\\(a\\(bs\\|ccept\\|tan2\\|larm\\|nd\\)\\|b\\(in\\(d\\|mode\\)\\|less\\)\\|c\\(h\\(r\\(\\|oot\\)\\|dir\\|mod\\|own\\)\\|aller\\|rypt\\|lose\\(\\|dir\\)\\|mp\\|o\\(s\\|n\\(tinue\\|nect\\)\\)\\)\\|CORE\\|d\\(ie\\|bm\\(close\\|open\\)\\|ump\\)\\|e\\(x\\(p\\|it\\|ec\\)\\|q\\|nd\\(p\\(rotoent\\|went\\)\\|hostent\\|servent\\|netent\\|grent\\)\\|of\\)\\|f\\(ileno\\|cntl\\|lock\\|or\\(k\\|mline\\)\\)\\|g\\(t\\|lob\\|mtime\\|e\\(\\|t\\(p\\(pid\\|r\\(iority\\|oto\\(byn\\(ame\\|umber\\)\\|ent\\)\\)\\|eername\\|w\\(uid\\|ent\\|nam\\)\\|grp\\)\\|host\\(by\\(addr\\|name\\)\\|ent\\)\\|s\\(erv\\(by\\(port\\|name\\)\\|ent\\)\\|ock\\(name\\|opt\\)\\)\\|c\\|login\\|net\\(by\\(addr\\|name\\)\\|ent\\)\\|gr\\(ent\\|nam\\|gid\\)\\)\\)\\)\\|hex\\|i\\(n\\(t\\|dex\\)\\|octl\\)\\|join\\|kill\\|l\\(i\\(sten\\|nk\\)\\|stat\\|c\\(\\|first\\)\\|t\\|e\\(\\|ngth\\)\\|o\\(caltime\\|g\\)\\)\\|m\\(sg\\(rcv\\|snd\\|ctl\\|get\\)\\|kdir\\)\\|n\\(e\\|ot\\)\\|o\\(pen\\(\\|dir\\)\\|r\\(\\|d\\)\\|ct\\)\\|p\\(ipe\\|ack\\)\\|quotemeta\\|r\\(index\\|and\\|mdir\\|e\\(quire\\|ad\\(pipe\\|\\|lin\\(k\\|e\\)\\|dir\\)\\|set\\|cv\\|verse\\|f\\|winddir\\|name\\)\\)\\|s\\(printf\\|qrt\\|rand\\|tat\\|ubstr\\|e\\(t\\(p\\(r\\(iority\\|otoent\\)\\|went\\|grp\\)\\|hostent\\|s\\(ervent\\|ockopt\\)\\|netent\\|grent\\)\\|ek\\(\\|dir\\)\\|lect\\|m\\(ctl\\|op\\|get\\)\\|nd\\)\\|h\\(utdown\\|m\\(read\\|ctl\\|write\\|get\\)\\)\\|y\\(s\\(read\\|call\\|tem\\|write\\)\\|mlink\\)\\|in\\|leep\\|ocket\\(pair\\|\\)\\)\\|t\\(runcate\\|ell\\(\\|dir\\)\\|ime\\(\\|s\\)\\)\\|u\\(c\\(\\|first\\)\\|time\\|mask\\|n\\(pack\\|link\\)\\)\\|v\\(alues\\|ec\\)\\|w\\(a\\(rn\\|it\\(pid\\|\\)\\|ntarray\\)\\|rite\\)\\|x\\(\\|or\\)\\|__\\(FILE__\\|LINE__\\|PACKAGE__\\)\\)\\>" 2
	     font-lock-type-face)
	    ("\\(^\\|[^$@%&\\]\\)\\<\\(AUTOLOAD\\|BEGIN\\|cho\\(p\\|mp\\)\\|d\\(e\\(fined\\|lete\\)\\|o\\)\\|DESTROY\\|e\\(ach\\|val\\|xists\\|ls\\(e\\|if\\)\\)\\|END\\|for\\(\\|each\\|mat\\)\\|g\\(rep\\|oto\\)\\|if\\|keys\\|l\\(ast\\|ocal\\)\\|m\\(ap\\|y\\)\\|n\\(ext\\|o\\)\\|p\\(ackage\\|rint\\(\\|f\\)\\|ush\\|o\\(p\\|s\\)\\)\\|q\\(\\|q\\|w\\|x\\)\\|re\\(turn\\|do\\)\\|s\\(pli\\(ce\\|t\\)\\|calar\\|tudy\\|ub\\|hift\\|ort\\)\\|t\\(r\\|ie\\)\\|u\\(se\\|n\\(shift\\|ti\\(l\\|e\\)\\|def\\|less\\)\\)\\|while\\|y\\|__\\(END\\|DATA\\)__\\|[sm]\\)\\>" 2
	     font-lock-other-type-face)
	    ("-[rwxoRWXOezsfdlpSbctugkTBMAC]\\>\\([ \t]+_\\>\\)?" 0
	     font-lock-function-name-face
	     keep)
	    ("\\<sub[ \t\n]+\\([a-zA-Z_:][a-zA-Z_0-9:]*\\([ \t\n]*([$;@\\%*&]*)\\)?\\)" 1 ;; Modified
	     font-lock-function-name-face)
	    ("\\<\\(package\\|require\\|use\\|no\\|import\\|unimport\\|new\\|bootstrap\\)[ \t]+\\([a-zA-Z_0-9:.]*\\)[ \t\n(;]" 2 ;; Modified
	     font-lock-function-name-face)
	    ("^[ \t]*format[ \t]+\\([a-zA-z_:][a-zA-z_0-9:]*\\)[ \t]*=[ \t]*$" 1
	     font-lock-function-name-face)
	    ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
	     (2 font-lock-string-face t)
	     ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil
	      nil
	      (1 font-lock-string-face t)))
	    ("[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1 font-lock-string-face t)
	    ("^[ \t]*\\([a-zA-Z0-9_]+[ \t]*:\\)[ \t]*\\($\\|{\\|\\<\\(until\\|while\\|for\\(each\\)?\\|do\\)\\>\\)" 1
	     font-lock-reference-face)
	    ("\\<\\(continue\\|next\\|last\\|redo\\|goto\\)\\>[ \t]+\\([a-zA-Z0-9_:]+\\)" 2
	     font-lock-reference-face)
	    ("\\(my\\|local\\)[ \t]*\\(([ \t]*\\)?\\([$@%*][a-zA-Z0-9_:]+\\)" (3 font-lock-variable-name-face)
	     ("\\=[ \t]*,[ \t]*\\([$@%*][a-zA-Z0-9_:]+\\)" nil
	      nil
	      (1 font-lock-variable-name-face)))
	    ("\\<for\\(each\\)?[ \t]*\\(\\$[a-zA-Z_:][a-zA-Z_0-9:]*\\)[ \t]*(" 2
	     font-lock-variable-name-face)
	    ("\\(\\([@%]\\|$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
	     (if (eq (char-after (match-beginning 2)) 37)
		 font-lock-other-emphasized-face
	       font-lock-emphasized-face)
	     keep)			;; Was T
	    ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
	     (if (= (- (match-end 2) (match-beginning 2)) 1)
		 (if (eq (char-after (match-beginning 3)) 123)
		     font-lock-other-emphasized-face
		   font-lock-emphasized-face)
	       font-lock-variable-name-face)
	     keep)			;; Was T
	    (,(q-delim-rx "'") 1 font-lock-string-face t) ;; Seq. added
	    (,(q-delim-rx "/") 1 font-lock-string-face t)
	    (,(q-delim-rx "|") 1 font-lock-string-face t)
	    (,(q-delim-rx "\"") 1 font-lock-string-face t)
	    (,(q-delim-rx "$") 1 font-lock-string-face t)
	    (,(q-delim-rx "!") 1 font-lock-string-face t)
	    (,(q-delim-rx ":") 1 font-lock-string-face t)
	    ;; Any others in common use?
	    (,(q-delim2-rx 2 "(" ")") 1 font-lock-string-face t)
	    (,(q-delim2-rx 2 "[" "]") 1 font-lock-string-face t)
	    (,(q-delim2-rx 2 "{" "}") 1 font-lock-string-face t)
	    (,(q-delim2-rx 2 "<" ">") 1 font-lock-string-face t)
	    ("\\(^\\|[ \t]\\)\\(#.*$\\)" 2 font-lock-comment-face t) ;; Added
	    ("^\\s-*###\\s-[^\n]*$" 0 'jglick-disco-directive t) ;; Added
	    )))))

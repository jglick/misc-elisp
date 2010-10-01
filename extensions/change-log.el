;;; $Id: change-log.el 1.1 Fri, 07 Feb 1997 20:27:06 +0100 jesse $

(eval-after-load
 "add-log"
 '(setq change-log-font-lock-keywords
	'(("^[SMTWF].+" . font-lock-comment-face)
	  ("^\t\\* \\([^:(\n]+\\)" 1 font-lock-reference-face)
	  ("(\\([^)\n]+\\)):" 1 font-lock-function-name-face)
	  ("\\*\\*\\* empty log message \\*\\*\\*" . font-lock-comment-face)
	  ("[^A-Za-z0-9_][`']\\([^`']+\\)'[^A-Za-z0-9_]" 1 font-lock-string-face))))

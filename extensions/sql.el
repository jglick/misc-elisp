;;; $Id: sql.el 1.3 Sat, 14 Nov 1998 06:31:45 +0100 dude $

;;; Optimal for Sybase.

;; ToDo:
;; Better detection of object names.
;; Italicized column lists.
;; Symbol-boundary vs. word-boundary detection.

(require 'cl)

(autoload 'sql-mode "sql-mode" "Major mode for editing SQL" t)
(push '("\\.m?sql\\'" . sql-mode) auto-mode-alist)
(add-hook 'sql-mode-hook
	  (lambda nil
	    (set-syntax-table sql-mode-syntax-table)))
(add-hook 'sql-mode-hook
	  (lambda nil
	    (make-local-variable 'font-lock-defaults)
	    (setq font-lock-defaults '((sql-font-lock-keywords) nil t))
	    (font-lock-mode 1)))
(add-hook 'sql-mode-hook
	  (lambda nil
	    (setq sql-magic-semicolon nil)
	    (setq sql-magic-go nil)))

(eval-after-load
 "sql-mode"
 '(progn
    (define-abbrev sql-mode-abbrev-table "abb" "abbreviated" nil)))

(defvar sql-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?(  "()" sql-mode-syntax-table)
(modify-syntax-entry ?)  ")(" sql-mode-syntax-table)
(modify-syntax-entry ?/  ". 14" sql-mode-syntax-table)
(modify-syntax-entry ?*  ". 23" sql-mode-syntax-table)
(modify-syntax-entry ?'  "\"" sql-mode-syntax-table)
(modify-syntax-entry ?\" "\"" sql-mode-syntax-table)
;; Should there be ^--.*$ comments here?

(defvar sql-font-lock-keywords
  ;; These lists are assuredly not complete, but look OK.
  '(("\\<\\(add\\|all\\|any\\|allocate\\|alter\\|and\\|as\\|asc\\|ascending\\|begin\\|between\\|break\\|browse\\|bulk\\|by\\|cascade\\|check\\|checkpoint\\|clustered\\|column\\|commit\\|compute\\|constraint\\|continue\\|copy\\|create\\|cursor\\|database\\|declare\\|default\\|delete\\|desc\\|descending\\|descriptor\\|disk\\|distinct\\|drop\\|dump\\|else\\|end\\|exec\\|execute\\|exists\\|for\\|foreign\\|from\\|goto\\|grant\\|group\\|having\\|holdlock\\|if\\|in\\|index\\|init\\|insert\\|into\\|is\\|key\\|kill\\|like\\|load\\|mirror\\|national\\|nonclustered\\|not\\|null\\|on\\|option\\|or\\|order\\|prepare\\|primary\\|print\\|proc\\|procedure\\|raiserror\\|read\\|reconfigure\\|reinit\\|remirror\\|return\\|revoke\\|rollback\\|rule\\|save\\|schema\\|select\\|set\\|setuser\\|shutdown\\|statistics\\|table\\|to\\|transaction\\|trigger\\|truncate\\|union\\|unique\\|unmirror\\|update\\|use\\|values\\|varying\\|view\\|waitfor\\|where\\|while\\|with\\|work\\|write\\)\\>" 0 font-lock-keyword-face t)
    ("\\<\\(binary\\|bit\\|char\\|character\\|datetime\\|decimal\\|double\\s-+precision\\|float\\|image\\|int\\|integer\\|money\\|nchar\\|numeric\\|nvarchar\\|real\\|smalldatetime\\|smallint\\|smallmoney\\|sysname\\|text\\|timestamp\\|tinyint\\|varbinary\\|varchar\\)\\>" 0 font-lock-type-face t)
    ("\\<\\(abs\\|acos\\|ascii\\|asin\\|atan\\|atn2\\|ceiling\\|char\\|charindex\\|col_length\\|col_name\\|column\\|convert\\|cos\\|cot\\|datalength\\|dateadd\\|datediff\\|datename\\|datepart\\|db_id\\|db_name\\|degrees\\|difference\\|exp\\|floor\\|getdate\\|host_id\\|host_name\\|index_col\\|isnull\\|log\\|log10\\|lower\\|ltrim\\|object_id\\|object_name\\|patindex\\|pi\\|power\\|radians\\|rand\\|replicate\\|reverse\\|right\\|round\\|rtrim\\|sign\\|sin\\|soundex\\|space\\|sqrt\\|str\\|stuff\\|substring\\|suser_id\\|suser_name\\|tan\\|textptr\\|textvalid\\|upper\\|user_id\\|user_name\\)\\s-*(" 1 font-lock-function-name-face t)
    ("@[@a-zA-Z_][a-zA-Z_0-9]*" 0 font-lock-variable-name-face t)
    ;; Object names...still missing GRANT stuff
    ("\\<\\(\\(\\(create\\|drop\\)\\(\\s-+\\(unique\\|nonclustered\\|clustered\\)\\)?\\s-+[a-zA-Z]+\\)\\|\\(insert\\(\\s-+into\\)?\\)\\|update\\|delete\\)\\s-+\\([a-zA-Z_#][a-zA-Z_0-9]*\\)" 8 font-lock-reference-face keep)
    ("\\<\\(from\\|use\\|on\\|to\\)\\s-+\\([a-zA-Z_#][a-zA-Z_0-9]*\\)" 2 font-lock-reference-face t)
    ("\\<\\([a-zA-Z_#][a-zA-Z_0-9]*\\)\\.[a-zA-Z_]" 1 font-lock-reference-face t)
    ;; Command terminator
    ("\\<go\\>" 0 'bold t)
    ;; Strings
    ("'\\([^']\\|''\\)*'" 0 font-lock-string-face t)
    ("\"\\([^\"]\\|\"\"\\)*\"" 0 font-lock-string-face t)
    ;; Comments
    ("/\\*\\([^*]\\|\\*[^*/]\\|\\*+[^*/]\\)*\\*+/" 0 font-lock-comment-face t)
    ("^--.*" 0 font-lock-comment-face t)
    ))

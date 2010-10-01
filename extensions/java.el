(require 'cl)

(push ".class" completion-ignored-extensions)

(add-hook 'java-mode-hook (lambda nil (setq c-hanging-comment-ender-p nil)))

(autoload 'java-properties-mode "extensions/java-properties-mode" "Major mode for editing Java Properties files." t)
(pushnew '("\\.properties\\(\\.[a-zA-Z0-9_]+\\)?\\'" . java-properties-mode)
	 auto-mode-alist
	 :test 'string-equal
	 :key 'car)

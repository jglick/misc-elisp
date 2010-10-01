;;; $Id: html-helper.el 1.6 Fri, 02 Oct 1998 20:53:32 +0200 dude $

(require 'cl)

(push '("\\.\\(.*html[^.]*\\|d2w\\)\\'" . html-helper-mode) auto-mode-alist)
(autoload 'html-helper-mode "html-helper-mode" "Major mode for editing HTML" t)

(add-hook 'html-helper-mode-hook '(lambda nil (auto-fill-mode 0)))
(setq html-helper-use-expert-menu t)

(setq html-helper-build-new-buffer t)
(setq html-helper-htmldtd-version " 3.0")
(setq html-helper-new-buffer-template
      '(
	"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML"
	html-helper-htmldtd-version
	"//EN\">\n" "<META NAME=\"Author\" CONTENT=\"" html-helper-address-string
	"\">\n"	"<HTML><HEAD>\n<TITLE>" p "</TITLE>\n</HEAD>\n" ;; "<BG BASE>"
	"<BODY BGCOLOR=\"#FFFFFF\">\n" p
	"\n</BODY>\n</HTML>\n"))

(setq html-helper-search-limit 25000)
(setq html-helper-do-write-file-hooks nil)

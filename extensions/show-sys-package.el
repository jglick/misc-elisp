;; show-sys-package.el

;; This Elisp package (designed for Emacs 20, might work on 19) is
;; designed to display a modeline annotation for any buffer which is
;; registered with the OS's software packaging system. The exact
;; displayed string depends on the package system, but should give a
;; name and maybe a version number. Now you can (1) know at once what
;; this mysterious config file is for and (2) know to think twice
;; before modifying it. The query of the package for a given file is
;; done asynchronously, so your load average might go up a smidgeon
;; but you will not have to wait after visiting every file.
;;
;; In principle, you can have more than one package system on the same
;; machine and all appropriate annotations will be displayed. This is
;; untested.
;;
;; Currently Irix inst-system, Linux RPM and Solaris pkg-commands are
;; supported. However, it is intended to be easy to plug in support
;; for new package systems; just add an entry to
;; `show-sys-package-possible-styles'; entries to the four alists at
;; the bottom of the file; and whatever functions you indicated.
;;
;; ToDo:
;;
;; Should have a user-configurable list of absolute directory prefixes
;; to automatically ignore, i.e. assume that no system packages would
;; have been installed in these places. This would substantially
;; reduce user objection to load, probably.
;;
;; Should put timer on asynch check; cf. prcs.el. In fact, currently
;; using this package screws up prcs.el's asynch checks completely,
;; for unknown reasons.
;;
;; Compile warnings:
;;  ** reference to free variable show-sys-package-styles
;;  ** reference to free variable show-sys-package-temp-displays
;;  ** reference to free variable show-sys-package-command-creators
;;While compiling show-sys-package-handler:
;;  ** reference to free variable show-sys-package-parsers

(require 'cl)

(defvar show-sys-package-possible-styles '(irix-inst rpm solaris-pkg)
  "List of possible package styles to check. DO NOT CHANGE AT
RUNTIME. Change `show-sys-package-styles' instead.")

(defvar show-sys-package-modeline-annotation nil
  "Variable which will contain package identifications for
particular file buffers.

It will be a list of the form of an empty string, then a list of
strings for each of the supported styles (so they may easily be set
individually).")
(make-variable-buffer-local 'show-sys-package-modeline-annotation)
(unless (assq 'show-sys-package-modeline-annotation minor-mode-alist)
  (push '(show-sys-package-modeline-annotation show-sys-package-modeline-annotation) minor-mode-alist))

(defvar show-sys-package-process-alist nil
  "Alist of processes to list of filename, catch-buffer & style (for
use by sentinels).")

(defvar show-sys-package-being-worked-on nil
 "Is this buffer currently being checked? Actually a list of currently
being-checked styles.")
(make-variable-buffer-local 'show-sys-package-being-worked-on)

(defun show-sys-package-annotate ()
  (when show-sys-package-styles
    (let ((filename (buffer-file-name)))
      (dolist (style show-sys-package-styles)
	(unless (memq style show-sys-package-being-worked-on)
	  (push style show-sys-package-being-worked-on)
	  (let* ((catcher (generate-new-buffer (concat " " (cdr (assq style show-sys-package-temp-displays))
						       " check for " filename)))
		 (process-connection-type nil)
		 (process (apply 'start-process
				 (append (list (concat (cdr (assq style show-sys-package-temp-displays)) ": " filename)
					       catcher)
					 (funcall (cdr (assq style show-sys-package-command-creators))
						  filename)))))
	    (process-kill-without-query process)
	    (set-process-sentinel process 'show-sys-package-handler)
	    (unless show-sys-package-modeline-annotation
	      (setq show-sys-package-modeline-annotation
		    (cons "" (make-list (length show-sys-package-possible-styles) nil))))
	    (setf (nth (1+ (position style show-sys-package-possible-styles))
		       show-sys-package-modeline-annotation)
		  (concat " " (cdr (assq style show-sys-package-temp-displays)) "?"))
	    (push (list process filename catcher style) show-sys-package-process-alist)))))))

(defun show-sys-package-handler (process *ignore*)
  (let ((info (cdr (assq process show-sys-package-process-alist))))
    (unless info
      (error "Weird--this apparently is not a package checker process"))
    (setq show-sys-package-process-alist (delete* process show-sys-package-process-alist :key 'car))
    (let* ((filename (car info))
	   (catcher (cadr info))
	   (style (caddr info))
	   (buffer (get-file-buffer filename)))
      (when buffer
	(save-excursion
	  (set-buffer buffer)
	  (setq show-sys-package-being-worked-on (delq style show-sys-package-being-worked-on))
	  (let ((how (process-status process)))
	    (unless (eq how 'exit)
	      (error "Package checker died abnormally")))
	  (setf (nth (1+ (position style show-sys-package-possible-styles))
		       show-sys-package-modeline-annotation)
		(let ((result
		       (funcall (cdr (assq style show-sys-package-parsers))
				(process-exit-status process)
				buffer
				(save-excursion (set-buffer catcher) (buffer-string)))))
		  (if result (concat " " result) nil)))
	  (kill-buffer catcher)
	  (delete-process process)
	  (force-mode-line-update))))))

(defvar show-sys-package-temp-displays
  '((rpm . "Redhat package")
    (solaris-pkg . "Solaris package")
    (irix-inst . "Irix instfile"))
  "Alist from package styles to descriptive names to display while checking.")
(defvar show-sys-package-enablers
  '((rpm . show-sys-package-rpm-enabler)
    (solaris-pkg . show-sys-package-solaris-enabler)
    (irix-inst . show-sys-package-irix-enabler))
  "Alist from package style to a test for enablement on this system.")
(defvar show-sys-package-command-creators
  '((rpm . show-sys-package-rpm-cmd)
    (solaris-pkg . show-sys-package-solaris-cmd)
    (irix-inst . show-sys-package-irix-cmd))
  "Alist from package style to a command creator; given an absolute
filename, should return a command, as a list of strings incl. command
name (in path or absolute) and arguments, to be run in background.")
(defvar show-sys-package-parsers
  '((rpm . show-sys-package-rpm-parser)
    (solaris-pkg . show-sys-package-solaris-parser)
    (irix-inst . show-sys-package-irix-parser))
  "Alist from package style to a parser for the command output; given the
process's numeric exit status, associated buffer, and text returned by
the command (stdout only), should return the modeline notation: either
nil for no status, or a string.")

(defun show-sys-package-rpm-enabler ()
  (eql 0
       (condition-case nil
	   (call-process "rpm" nil nil nil "--version")
	 (file-error nil))))

(defun show-sys-package-rpm-cmd (filename) (list "rpm" "-q" "-f" filename))

(defun show-sys-package-rpm-parser (status buffer result)
  (case status
    ;; XXX files in >1 package will show up with `^J' in here. Not
    ;; common.
    (0 (substring result 0 -1))
    (1 nil)
    (t (error "Weird result for command: %S" status))))

(defun show-sys-package-irix-enabler ()
  (and (string-match "irix" system-configuration)
       (file-executable-p "/usr/sbin/showfiles")))

(defun show-sys-package-irix-cmd (filename)
  ;; XXX regexp-quote is not quite right but close. Actually want to
  ;; quote just those characters expected by Irix regexp(5), but _not_
  ;; dot. Close enough though.
  ;; XXX -C shows modified status, but need -l (default) to show
  ;; package. No way to get both at the same time (stoopid). Modified
  ;; status is maybe not so important, but would be a welcome
  ;; addition. Easiest fix might be to split up Irix "style" into two
  ;; halves, one for each check (ecch).
  (list "/usr/sbin/showfiles" "--" (concat (regexp-quote filename) "$")))

(defun show-sys-package-irix-parser (status buffer result)
  (case status
    (0 (if (zerop (length result))
	   ;; No such inst file.
	   nil
	 (nth 3 (split-string result))))
    (t (error "Weird result for command: %S" status))))

(defun show-sys-package-solaris-enabler ()
  (and (string-match "solaris" system-configuration)
       (file-executable-p "/usr/sbin/pkgchk")))

(defun show-sys-package-solaris-cmd (filename)
  (list "/usr/sbin/pkgchk" "-l" "-p" filename))

;; XXX might be nice to do a sum(1) to check for modification
;; XXX Solaris package names are very cryptic; option to use
;; "description" might be nice, but those are typically too long!
(defun show-sys-package-solaris-parser (status buffer result)
  (case status
    (0 (if (zerop (length result)) nil
	 ;; XXX only notices the first package, if file belongs to >1
	 (if (string-match "Referenced by the following packages:[ \t\n]+\\([^ \t\n]+\\)" result)
	     (substring result (match-beginning 1) (match-end 1))
	   nil)))
    (t (error "Weird result for command: %S" status))))

(defvar show-sys-package-styles
  (remove-if-not (lambda (style)
		   (funcall (cdr (assq style show-sys-package-enablers))))
		 show-sys-package-possible-styles)
  "*Style of package system to check. Currently supports Irix INST
`irix-inst', (usually Linux) RPM `rpm', and Solaris
`solaris-pkg'. Should be a list of styles to check, or `nil' to
disable. Will hopefully be autodetected.")
(add-hook 'find-file-hooks 'show-sys-package-annotate)

(provide 'show-sys-package)

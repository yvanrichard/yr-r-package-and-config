;;; stan-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "stan-keywords" "stan-keywords.el" (0 0 0 0))
;;; Generated autoloads from stan-keywords.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "stan-keywords" '("stan-keywords--")))

;;;***

;;;### (autoloads nil "stan-mode" "stan-mode.el" (0 0 0 0))
;;; Generated autoloads from stan-mode.el

(autoload 'stan-mode "stan-mode" "\
A major mode for editing Stan files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `stan-mode-hook'.

Key bindings:
\\{stan-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist `(,(rx (seq ".stan" eos)) . stan-mode))

(autoload 'stan-mode-setup "stan-mode" "\
Set up comment and indent style for `stan-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "stan-mode" '("stan-")))

;;;***

;;;### (autoloads nil nil ("stan-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; stan-mode-autoloads.el ends here

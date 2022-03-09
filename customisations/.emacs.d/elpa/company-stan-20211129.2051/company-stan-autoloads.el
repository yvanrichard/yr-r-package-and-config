;;; company-stan-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-stan" "company-stan.el" (0 0 0 0))
;;; Generated autoloads from company-stan.el

(autoload 'company-stan-setup "company-stan" "\
Set up `company-stan-backend'.

Add `company-stan-backend' to `company-backends' buffer locally.

It is grouped with `company-dabbrev-code' because `company-stan-backend'
only performs completion based on predefined keywords in
`company-stan-keyword-list'.  See the help for `company-backends' for
details regarding grouped backends.

Add this function to the `stan-mode-hook'." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-stan" '("company-stan-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-stan-autoloads.el ends here

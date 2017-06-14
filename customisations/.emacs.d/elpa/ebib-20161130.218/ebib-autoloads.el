;;; ebib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ebib" "ebib.el" (22591 18457 670376 846000))
;;; Generated autoloads from ebib.el

(autoload 'ebib "ebib" "\
Ebib, a BibTeX database manager.
Optional argument FILE is a file to load.  If FILE is already
loaded, switch to it.  If KEY is given, jump to it.

\(fn &optional FILE KEY)" t nil)

(autoload 'ebib-show-entry "ebib" "\
Open Ebib and jump to KEY.

\(fn KEY)" nil nil)

(defalias 'ebib-open-org-link 'ebib-show-entry "\
Open Ebib and jump to KEY.
This is for use in Org-mode links.")

;;;***

;;;### (autoloads nil nil ("ebib-db.el" "ebib-filters.el" "ebib-keywords.el"
;;;;;;  "ebib-notes.el" "ebib-pkg.el" "ebib-reading-list.el" "ebib-utils.el")
;;;;;;  (22591 18457 566379 917000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ebib-autoloads.el ends here
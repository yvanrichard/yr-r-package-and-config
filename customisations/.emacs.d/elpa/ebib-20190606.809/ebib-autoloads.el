;;; ebib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ebib" "ebib.el" (23809 41765 687566 53000))
;;; Generated autoloads from ebib.el

(autoload 'ebib "ebib" "\
Ebib, a BibTeX database manager.
Optional argument FILE is a file to load.  If FILE is already
loaded, switch to it.  If KEY is given, jump to it.

\(fn &optional FILE KEY)" t nil)

(autoload 'ebib-init "ebib" "\
Initialise Ebib.
This function sets all variables to their initial values, creates
the buffers and loads the files in `ebib-preload-bib-files'.

This function can be used to open Ebib in the background, e.g.,
in the user's init file.

\(fn)" nil nil)

(autoload 'ebib-insert-citation "ebib" "\
Insert a citation at POINT.
The user is prompted for a BibTeX key and has to choose one from
the database(s) associated with the current buffer (see
`ebib--get-local-bibfiles' for details), or from the current
database if the current buffer has no associated databases.

This is a front-end for other citation insertion functions: if
the `ivy' package is loaded, it calls `ebib-insert-citation-ivy',
if the `helm' package is loaded, it calls
`ebib-insert-citation-helm', otherwise it calls
`ebib-insert-citation-default-method', which uses standard Emacs
completion.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "org-ebib" "org-ebib.el" (23809 41765 675565
;;;;;;  949000))
;;; Generated autoloads from org-ebib.el

(autoload 'org-ebib-open "org-ebib" "\
Open Ebib and jump to KEY.

\(fn KEY)" nil nil)

;;;***

;;;### (autoloads nil nil ("ebib-db.el" "ebib-filters.el" "ebib-keywords.el"
;;;;;;  "ebib-notes.el" "ebib-pkg.el" "ebib-reading-list.el" "ebib-utils.el")
;;;;;;  (23809 41765 691566 88000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ebib-autoloads.el ends here
;;; helm-navi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "helm-navi" "helm-navi.el" (22874 53369 888249
;;;;;;  880000))
;;; Generated autoloads from helm-navi.el

(defalias 'helm-navi 'helm-navi-headings-and-keywords-current-buffer)

(autoload 'helm-navi-headings-and-keywords-current-buffer "helm-navi" "\
Show matches for all `navi-mode' keywords and `outshine' headings in current buffer.

\(fn)" t nil)

(defalias 'helm-navi-headings 'helm-navi-headings-current-buffer)

(autoload 'helm-navi-headings-current-buffer "helm-navi" "\
Show matches for Outshine headings in current buffer.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-navi-autoloads.el ends here

;;; poly-R-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "poly-R" "poly-R.el" (23697 27887 144334 556000))
;;; Generated autoloads from poly-R.el
 (autoload 'poly-noweb+R-mode "poly-R")

(defalias 'poly-noweb+r-mode 'poly-noweb+R-mode)
 (autoload 'poly-markdown+R-mode "poly-R")

(defalias 'poly-markdown+r-mode 'poly-markdown+R-mode)
 (autoload 'poly-rapport-mode "poly-R")
 (autoload 'poly-html+R-mode "poly-R")

(defalias 'poly-html+r-mode 'poly-html+R-mode)
 (autoload 'poly-brew+R-mode "poly-R")

(defalias 'poly-brew+r-mode 'poly-brew+R-mode)
 (autoload 'poly-R+C++-mode "poly-R")

(defalias 'poly-r+c++-mode 'poly-R+C++-mode)
 (autoload 'poly-C++R-mode "poly-R")

(defalias 'poly-c++r-mode 'poly-C++R-mode)
 (autoload 'poly-ess-help+R-mode "poly-R")
 (autoload 'poly-Rd-mode "poly-R")

(add-to-list 'auto-mode-alist '("\\.Snw\\'" . poly-noweb+r-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . poly-noweb+r-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-markdown+r-mode))

(add-to-list 'auto-mode-alist '("\\.rapport\\'" . poly-rapport-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]html\\'" . poly-html+r-mode))

(add-to-list 'auto-mode-alist '("\\.[rR]brew\\'" . poly-brew+r-mode))

(add-to-list 'auto-mode-alist '("\\.[Rr]cpp\\'" . poly-r+c++-mode))

(add-to-list 'auto-mode-alist '("\\.cpp[rR]\\'" . poly-c++r-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; poly-R-autoloads.el ends here

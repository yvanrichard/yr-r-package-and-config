;;; ivy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ivy-mode) "ivy" "ivy.el" (21803 1676 238074 614000))
;;; Generated autoloads from ivy.el

(defvar ivy-mode nil "\
Non-nil if Ivy mode is enabled.
See the command `ivy-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.")

(custom-autoload 'ivy-mode "ivy" nil)

(autoload 'ivy-mode "ivy" "\
Toggle Ivy mode on or off.
With ARG, turn Ivy mode on if arg is positive, off otherwise.
Turning on Ivy mode will set `completing-read-function' to
`ivy-completing-read'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("ivy-pkg.el") (21803 1676 326945 227000))

;;;***

(provide 'ivy-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-autoloads.el ends here

;;; hl-block-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hl-block-mode" "hl-block-mode.el" (0 0 0 0))
;;; Generated autoloads from hl-block-mode.el

(autoload 'hl-block-mode "hl-block-mode" "\
Highlight block under the cursor.

If called interactively, enable Hl-Block mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-hl-block-mode 'globalized-minor-mode t)

(defvar global-hl-block-mode nil "\
Non-nil if Global Hl-Block mode is enabled.
See the `global-hl-block-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hl-block-mode'.")

(custom-autoload 'global-hl-block-mode "hl-block-mode" nil)

(autoload 'global-hl-block-mode "hl-block-mode" "\
Toggle Hl-Block mode in all buffers.
With prefix ARG, enable Global Hl-Block mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Hl-Block mode is enabled in all buffers where
`hl-block-mode-turn-on' would do it.
See `hl-block-mode' for more information on Hl-Block mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hl-block-mode" '("hl-block-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hl-block-mode-autoloads.el ends here

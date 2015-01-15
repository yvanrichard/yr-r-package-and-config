;;; anchored-transpose-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anchored-transpose) "anchored-transpose" "anchored-transpose.el"
;;;;;;  (21674 1857 920865 918000))
;;; Generated autoloads from anchored-transpose.el

(autoload 'anchored-transpose "anchored-transpose" "\
Transpose portions of the region around an anchor phrase.

`this phrase but not that word'    can be transposed into
`that word but not this phrase'

I want this phrase but not that word.
       |----------------------------|. .This is the entire phrase.
                  |-------|. . . . . . .This is the anchor phrase.

First select the entire phrase and type \\[anchored-transpose].  Then select
the anchor phrase and type \\[anchored-transpose] again.  By default the
anchor phrase will automatically include any surrounding whitespace even if
you don't explicitly select it.  Also, it won't include certain trailing
punctuation.  See `anchored-transpose-do-fuzzy' for details.  A prefix arg
prior to either selection means `no fuzzy logic, use selections literally'.

You can select the anchor phrase first followed by the entire phrase if more
convenient.  Typing \\[anchored-transpose] with nothing selected clears any
prior selection.  If both primary and secondary selections are active this
command swaps the 2 selections immediately.

I want this phrase but not that word.
       |----------|       |---------|   Separate phrase selection.

You can also select the phrases to be swapped separately in any order.

\(fn BEG1 END1 FLG1 &optional BEG2 END2 FLG2)" t nil)

;;;***

;;;### (autoloads nil nil ("anchored-transpose-pkg.el") (21674 1858
;;;;;;  72050 911000))

;;;***

(provide 'anchored-transpose-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; anchored-transpose-autoloads.el ends here

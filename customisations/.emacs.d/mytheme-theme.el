(deftheme mytheme
  "Created 2015-03-18.")

(custom-theme-set-variables
 'mytheme
 '(LaTeX-biblatex-use-Biber t)
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ess-R-font-lock-keywords (quote ((ess-R-fl-keyword:modifiers . t) (ess-R-fl-keyword:fun-defs . t) (ess-R-fl-keyword:keywords . t) (ess-R-fl-keyword:assign-ops . t) (ess-R-fl-keyword:constants . t) (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t) (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t) (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t))))
 '(hl-sexp-background-color "#251D25")
 '(inhibit-startup-screen t)
 '(markdown-command "pandoc --smart -f markdown -t html")
 '(markdown-css-path "/home/yvan/Documents/css/github-markdown.css")
 '(safe-local-variable-values (quote ((TeX-master . "report.tex") (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark) (TeX-master . "report") (TeX-master . report\.tex) (require-final-newline))))
 '(sml/replacer-regexp-list (quote (("^~/org" ":Org:") ("^~/\\.emacs\\.d/" ":ED:") ("^/sudo:.*:" ":SU:") ("^~/Documents/" ":Doc:") ("^~/Dropbox/" ":DB:") ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:") ("^~/[Gg]it/" ":Git:") ("^~/[Gg]it[Hh]ub/" ":Git:") ("^~/dragonfly/yr-r-package-and-config/" ":YRPKG:") ("^~/dragonfly/npoa-observer-optimisation/" ":NPOA:") ("^~/dragonfly/sra-2014/" ":SRA14:") ("^~/dragonfly/" ":DFLY:") ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))))
 '(sml/shorten-directory t)
 '(wakatime-api-key "0ff58d48-ac18-40ee-be06-f0e1c5985c86")
 '(wakatime-cli-path "/home/yvan/wakatime/wakatime-cli.py")
 '(yank-pop-change-selection t)
 '(custom-safe-themes (quote ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "6c9ddb5e2ac58afb32358def7c68b6211f30dec8a92e44d2b9552141f76891b3" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "08efabe5a8f3827508634a3ceed33fa06b9daeef9c70a24218b70494acdf7855" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))))

(custom-theme-set-faces
 'mytheme
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :family "Source Code Pro"))))
 '(ess-numbers-face ((t (:inherit font-lock-type-face :foreground "light salmon" :slant normal))))
 '(font-lock-builtin-face ((t (:foreground "PeachPuff"))))
 '(font-lock-comment-face ((t (:foreground "maroon"))))
 '(font-lock-constant-face ((t (:foreground "goldenrod1"))))
 '(font-lock-keyword-face ((t (:foreground "khaki1"))))
 '(font-lock-string-face ((t (:foreground "DarkSeaGreen2"))))
 '(font-lock-type-face ((t (:foreground "pale green"))))
 '(highlight-indentation-face ((t (:inherit fringe :background "gray16"))))
 '(org-level-4 ((t (:foreground "khaki1" :inherit (outline-4)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "darksalmon"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "LightGoldenrod4"))))
 '(region ((t (:background "grey50")))))

(provide-theme 'mytheme)

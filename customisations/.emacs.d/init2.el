;; set load path to custom lisp and themes
(setq custom-lisp-dir
      (expand-file-name "custom-lisp" user-emacs-directory))
(add-to-list 'load-path custom-lisp-dir)

;; set package archives
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")))

(package-initialize)


;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package bind-key
  :ensure t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

;; validate options
(use-package validate
  :ensure t
  :init
  (use-package seq
    :ensure t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-biblatex-use-Biber t t)
 '(auto-fill-inhibit-regexp "\\\\Sexpr\\\\{.*\\\\}")
 '(avy-timeout-seconds 0.35)
 '(blink-cursor-blinks 1)
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output t)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comment-style 'indent)
 '(completion-ignored-extensions
   '(".hi" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".r~"))
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(delete-by-moving-to-trash t)
 '(diredp-hide-details-initially-flag nil)
 '(doc-view-resolution 200)
 '(doom-modeline-buffer-file-name-style 'auto)
 '(dumb-jump-mode t)
 '(dumb-jump-prefer-searcher 'ag)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords . t)
	 (ess-R-fl-keyword:constants . t)
	 (ess-R-fl-keyword:modifiers . t)
	 (ess-R-fl-keyword:fun-defs . t)
	 (ess-R-fl-keyword:assign-ops . t)
	 (ess-R-fl-keyword:%op% . t)
	 (ess-fl-keyword:fun-calls . t)
	 (ess-fl-keyword:numbers . t)
	 (ess-fl-keyword:operators . t)
	 (ess-fl-keyword:delimiters)
	 (ess-fl-keyword:= . t)
	 (ess-R-fl-keyword:F&T . t)))
 '(ess-eval-visibly nil)
 '(ess-gen-proc-buffer-name-function 'ess-gen-proc-buffer-name:project-or-directory)
 '(ess-pdf-viewer-pref "okular")
 '(ess-r-args-electric-paren t)
 '(fci-rule-color "#873b81")
 '(fill-column 90)
 '(fill-prefix nil)
 '(flyspell-default-dictionary "en_GB")
 '(font-latex-fontify-sectioning 1.05)
 '(global-prettify-symbols-mode t)
 '(highlight-parentheses-attributes '((weight bold)))
 '(highlight-parentheses-background-colors '("black"))
 '(highlight-parentheses-colors '("yellow" "IndianRed1" "IndianRed3" "IndianRed4"))
 '(hl-sexp-background-color "#171717")
 '(ibuffer-filter-group-name-face 'compilation-info)
 '(ido-ignore-extensions nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ispell-local-dictionary "en_GB")
 '(markdown-command "pandoc --smart -f markdown -t html")
 '(org-babel-R-command "R --vanilla --slave --no-save")
 '(org-babel-load-languages '((latex . t) (R . t) (dot . t) (lisp . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-html-use-infojs t)
 '(org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))
 '(outshine-fontify-whole-heading-line nil)
 '(outshine-preserve-delimiter-whitespace nil)
 '(outshine-startup-folded-p nil)
 '(package-selected-packages
   '(quarto-mode vterm anchored-transpose projectile git-timemachine hl-block-mode dimmer highlight-sexp rainbow-mode google-this yasnippet-snippets helpful which-key dired+ highlight-parentheses navi-mode yasnippet avy expand-region rainbow-delimiters ess doom-modeline psession magit stan-mode poly-R markdown-mode validate use-package))
 '(projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")
 '(protect-buffer-bury-p nil)
 '(safe-local-variable-values
   '((TeX-master . \./report\.tex)
	 (TeX-master . t)
	 (TeX-master . "report.tex")
	 (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
	 (TeX-master . "report")
	 (TeX-master . report\.tex)
	 (require-final-newline)))
 '(search-whitespace-regexp "[ \\t\\r\\n]+")
 '(show-paren-style 'expression)
 '(sml/replacer-regexp-list
   '(("^~/org" ":Org:")
	 ("^~/\\.emacs\\.d/" ":ED:")
	 ("^/sudo:.*:" ":SU:")
	 ("^~/Documents/" ":Doc:")
	 ("^~/Dropbox/" ":DB:")
	 ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
	 ("^~/[Gg]it/" ":Git:")
	 ("^~/[Gg]it[Hh]ub/" ":Git:")
	 ("^~/dragonfly/yr-r-package-and-config/" ":YRPKG:")
	 ("^~/dragonfly/npoa-observer-optimisation/" ":NPOA:")
	 ("^~/dragonfly/sra-2014/" ":SRA14:")
	 ("^~/dragonfly/sra-2016/" ":SRA16:")
	 ("^~/dragonfly/" ":DFLY:")
	 ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")))
 '(sml/shorten-directory t)
 '(tags-table-list '("/home/yvan/Documents/TAGS"))
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#cc6666")
	 (40 . "#de935f")
	 (60 . "#f0c674")
	 (80 . "#b5bd68")
	 (100 . "#8abeb7")
	 (120 . "#81a2be")
	 (140 . "#b294bb")
	 (160 . "#cc6666")
	 (180 . "#de935f")
	 (200 . "#f0c674")
	 (220 . "#b5bd68")
	 (240 . "#8abeb7")
	 (260 . "#81a2be")
	 (280 . "#b294bb")
	 (300 . "#cc6666")
	 (320 . "#de935f")
	 (340 . "#f0c674")
	 (360 . "#b5bd68")))
 '(vc-annotate-very-old-color nil)
 '(wakatime-api-key "f0473578-ab9e-489f-a2e0-f55e7e9ff5ed")
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(yank-pop-change-selection t)
 '(yas-also-auto-indent-first-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#202020" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "JB" :family "JetBrains Mono"))))
 '(auto-dim-other-buffers-face ((t (:background "#000" :foreground "gray"))))
 '(comint-highlight-input ((t (:foreground "#66BB66"))))
 '(comint-highlight-prompt ((t (:inherit minibuffer-prompt :foreground "#009900"))))
 '(compilation-info ((t (:inherit success :foreground "SpringGreen3"))))
 '(dired-rainbow-data-face ((t (:foreground "#FFDDAA"))))
 '(dired-rainbow-image-face ((t (:foreground "#DDAABB"))))
 '(dired-rainbow-program-face ((t (:foreground "burlywood"))))
 '(dired-rainbow-r-face ((t (:foreground "#00FF55" :weight bold))))
 '(diredp-date-time ((t (:foreground "#DDDDFF"))))
 '(diredp-dir-name ((t (:background "#000000" :foreground "#FF0000" :slant italic :weight bold))))
 '(diredp-dir-priv ((t (:background "#000000" :foreground "#CCCCFF" :weight bold))))
 '(diredp-file-name ((t (:foreground "white"))))
 '(diredp-file-suffix ((t (:foreground "#FFFFAA"))))
 '(diredp-flag-mark-line ((t (:background "#333333" :foreground "#FFFF00"))))
 '(diredp-ignored-file-name ((t (:foreground "#444444"))))
 '(diredp-number ((t (:foreground "#FFFFBB"))))
 '(diredp-other-priv ((t (:background "#333355"))))
 '(diredp-read-priv ((t (:background "#443333"))))
 '(diredp-write-priv ((t (:background "#334433"))))
 '(ediff-even-diff-A ((t (:background "tomato4"))))
 '(ediff-even-diff-B ((t (:background "tomato4"))))
 '(ediff-odd-diff-A ((t (:background "sienna4"))))
 '(ediff-odd-diff-B ((t (:background "sienna"))))
 '(ess-assignment-face ((t (:inherit font-lock-constant-face :foreground "#FFFF55"))))
 '(ess-modifiers-face ((t (:inherit font-lock-constant-face))))
 '(ess-numbers-face ((t (:foreground "#DDAADD" :weight normal))))
 '(ess-operator-face ((t (:inherit font-lock-constant-face))))
 '(ess-r-control-flow-keyword-face ((t (:inherit ess-keyword-face))))
 '(eww-form-submit ((t (:background "#444444" :foreground "#EEEEEE" :box (:line-width 2 :style released-button)))))
 '(eww-form-textarea ((t (:background "#C0C0C0" :foreground "black" :box 1))))
 '(fixed-pitch ((t (:height 1.01 :family "JetBrains Mono Medium"))))
 '(flx-highlight-face ((t (:inherit font-lock-variable-name-face :underline "#666666" :weight bold))))
 '(flyspell-duplicate ((t (:foreground "indian red" :underline nil))))
 '(font-latex-sectioning-2-face ((t (:foreground "yellow" :weight bold :height 1.3 :family "ubuntu-mono"))))
 '(font-latex-sectioning-3-face ((t (:foreground "gold" :weight bold :height 1.1 :width condensed :family "DejaVu Sans"))))
 '(font-latex-sectioning-4-face ((t (:foreground "LightGoldenrod1" :weight bold :width condensed :family "DejaVu Sans"))))
 '(font-latex-sedate-face ((t (:foreground "light coral"))))
 '(font-latex-warning-face ((t (:foreground "orange red"))))
 '(font-lock-builtin-face ((t (:foreground "PeachPuff"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#707070"))))
 '(font-lock-constant-face ((t (:inherit default :foreground "#77DD88"))))
 '(font-lock-function-name-face ((t (:foreground "#DDEEFF"))))
 '(font-lock-keyword-face ((t (:foreground "#88DD88"))))
 '(font-lock-string-face ((t (:foreground "#CCEECC"))))
 '(font-lock-type-face ((t (:foreground "yellow green"))))
 '(font-lock-warning-face ((t (:background "yellow" :foreground "red" :weight semi-bold))))
 '(hi-yellow ((t (:background "yellow1" :foreground "red"))))
 '(highlight ((t (:background "#552222"))))
 '(highlight-indentation-face ((t (:inherit fringe :background "gray30"))))
 '(highlight-parentheses-highlight ((t nil)) t)
 '(italic ((t (:slant italic :height 1.0))))
 '(link ((t (:foreground "#CCDDFF" :underline "#110011"))))
 '(markdown-header-face ((t (:inherit outline-1 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit outline-2 :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit outline-3))))
 '(match ((t (:background "#224477"))))
 '(org-block ((t (:inherit shadow :foreground "#AAFFAA"))))
 '(org-code ((t (:inherit shadow :foreground "#AAFFAA"))))
 '(org-date ((t (:foreground "#AACCFF" :underline t))))
 '(org-level-1 ((t (:foreground "#FFF7BC"))))
 '(org-level-2 ((t (:foreground "#FCBBA1"))))
 '(org-level-3 ((t (:foreground "#C6DBEF"))))
 '(org-level-4 ((t (:foreground "#CCEBC5"))))
 '(org-special-keyword ((t (:foreground "#66FFFF"))))
 '(outline-1 ((t (:background "#000" :foreground "#FF8266" :box (:line-width 1 :color "#444") :slant italic :weight normal))))
 '(outline-2 ((t (:background "#090909" :foreground "#F7A06B" :box (:line-width 1 :color "grey20") :slant italic))))
 '(outline-3 ((t (:background "#191919" :foreground "#EEB96F" :box (:line-width 1 :color "grey20") :slant italic))))
 '(outline-4 ((t (:inherit font-lock-comment-face :background "#191919" :foreground "#E6CC73" :slant italic))))
 '(outline-5 ((t (:background "#191919" :foreground "#DDDB76" :slant italic))))
 '(outline-6 ((t (:background "#191919" :foreground "#C4D578" :slant italic))))
 '(outline-7 ((t (:background "#191919" :foreground "#ADCC7A" :slant italic))))
 '(popup-scroll-bar-foreground-face ((t (:background "red"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff7777"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#e69333"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffff88"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#66ff66"))))
 '(region ((t (:background "grey20"))))
 '(show-paren-match ((t (:inherit font-lock-type-face :background "#000000" :slant italic :weight ultra-bold))))
 '(show-paren-match-expression ((t (:background "black"))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(underline ((t (:underline "#666666"))))
 '(variable-pitch ((t (:height 1.01 :family "ETBembo, BoldLF"))))
 '(widget-button ((t (:foreground "deep sky blue" :weight bold)))))

(use-package wakatime-mode
  :ensure t
  :init
  (global-wakatime-mode)
  )



;; ibuffer

(use-package ibuffer
  :ensure t
  )

(global-set-key "\C-x\C-b" 'ibuffer)
(setq ibuffer-default-sorting-mode 'filename/process)

;; set sorting column
(setq Buffer-menu-sort-column 4)



;;; eshell configuration
(use-package eshell
  :ensure t
  :init
  (require 'em-smart)                        ; em-smart: https://www.masteringemacs.org/article/complete-guide-mastering-eshell
  :config
  (validate-setq
   eshell-where-to-jump 'begin
   eshell-review-quick-commands nil
   eshell-smart-space-goes-to-end t))


(use-package hippie-exp                 ; Powerful expansion and completion
  :bind ("C-\\" . hippie-expand)
  :config
  (progn
    (validate-setq hippie-expand-try-functions-list
                   '(try-expand-dabbrev
                     try-expand-dabbrev-all-buffers
                     try-expand-dabbrev-from-kill
                     try-complete-file-name-partially
                     try-complete-file-name
                     try-expand-all-abbrevs
                     try-expand-list
                     try-expand-line
                     try-complete-lisp-symbol-partially
                     try-complete-lisp-symbol))))


;; (use-package recentf                    ; Save recently visited files
;;   :init (recentf-mode)
;;   :config
;;   (validate-setq
;;    recentf-max-saved-items 500
;;    recentf-max-menu-items 15
;;    ;; Cleanup recent files only when Emacs is idle, but not when the mode
;;    ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
;;    ;; idles often enough to have the recent files list clean up regularly
;;    recentf-auto-cleanup 300
;;    recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
;;                          "/elpa/.*\\'"       ; Package files
;;                          "/itsalltext/"      ; It's all text temp files
;; 						 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * LSP mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-mode
;;   :ensure t
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   :init
;;   (setq lsp-keymap-prefix "s-l")
;;   :bind (:map lsp-mode-map
;;               ("C-c C-d C-w" . lsp-describe-thing-at-point))
;;   ;; :hook (
;;   ;;        (ess-r-mode . lsp)
;;   ;;        ;; if you want which-key integration
;;   ;;        (lsp-mode . lsp-enable-which-key-integration)
;;   ;;        )
;;   :commands lsp
;;   :config
;;   (setq lsp-enable-snippet t
;;         lsp-prefer-flymake nil
;;         )
;;   ;; (setq lsp-eldoc-hook '(lsp-hover))
;;   (remove-hook 'lsp-eldoc-hook 'lsp-document-highlight)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * ESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ess
  :ensure t
  :init (require 'ess-site)
  ;; ;; key binding for insert-assign that doesn't add extra spaces:
  ;; (defun r-insert-assign-space-aware ()
  ;;   (interactive)
  ;;   (just-one-space 1)
  ;;   (insert "<-")
  ;;   (just-one-space 1))
  ;; ;; key binding for pipe:
  ;; (defun r-pipe-operator ()
  ;;   (interactive)
  ;;   (just-one-space 1)
  ;;   (insert "%>%")
  ;;   (just-one-space 1))
  ;; ;; key binding for evaluating line or selected text:
  ;; (defun r-eval-line-or-selected ()
  ;;   (interactive)
  ;;   (if (and transient-mark-mode mark-active)
  ;;       (call-interactively 'ess-eval-region)
  ;;     (call-interactively 'ess-eval-line)))
  :mode (
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         )
  :interpreter (
                ("R" . R-mode)
                ("R" . R-transcript-mode)
                ("R" . Rd-mode)
                )
  
  ;; :bind (:map ess-r-mode-map
  ;;        ("s-n" . r-insert-assign-space-aware)
  ;;        ("s-N" . r-pipe-operator)
  ;;        ("s-m" . r-eval-line-or-selected)
  ;;        ("s-M" . ess-eval-region-or-function-or-paragraph-and-step)
  ;;        :map inferior-ess-r-mode-map
  ;;        ("s-n" . r-insert-assign-space-aware)
  ;;        ("s-N" . r-pipe-operator)
  ;;        ("s-m" . r-eval-line-or-selected)
  ;;        ("s-M" . ess-eval-region-or-function-or-paragraph-and-step))
  :config
  (use-package ess-r-mode
    :load-path "elpa/ess/")
  (validate-setq
   ring-bell-function #'ignore
   ess-ask-for-ess-directory nil
   ;;inferior-R-program-name "/usr/local/bin/R"
   ;; inferior-R-program-name "/Users/sejdemyr/xp-env/bin/xp-R"
   ess-local-process-name "R"
   ansi-color-for-comint-mode 'filter
   comint-scroll-to-bottom-on-input t
   comint-scroll-to-bottom-on-output t
   comint-move-point-for-output t
   ;; ess-smart-S-assign-key nil
   ess-style 'RStudio
   ;; ess-r-backend 'lsp
   ess-eval-visibly 'nowait
   )         ; rstudio indentation style
  :hook
  ;; ;; enable lintr
  ;; (ess-mode . (lambda ()
  ;;               (flycheck-mode t)))
  ;; ess-shift-enter to execute code
  (ess-mode . (lambda()
                (local-set-key [(shift return)] 'my-ess-eval)))
  (inferior-ess-mode . (lambda()
                         (local-set-key [C-up] 'comint-previous-input)
                         (local-set-key [C-down] 'comint-next-input)))
  (Rnw-mode . (lambda()
                (local-set-key [(shift return)] 'my-ess-eval)))
  ;; (ess-mode . company-mode)
  ;; (inferior-ess-mode . company-mode)
  )
;; (setq ess-smart-S-assign-key nil)
(define-key ess-mode-map (kbd ";") 'ess-insert-assign)
(define-key inferior-ess-mode-map (kbd ";") 'ess-insert-assign)


(defun back-to-indentation-or-beginning () (interactive)
   (if (= (point) (progn (back-to-indentation) (point)))
       (beginning-of-line)))
(global-set-key "\C-a" 'back-to-indentation-or-beginning)


;;; Markdown: markdown-mode
;; http://jblevins.org/projects/markdown-mode/
;; & https://github.com/basille/.emacs.d/blob/master/init.el
(use-package markdown-mode
  :ensure t				; Check and install if necessary
  :commands markdown-mode		; Autoloads for markdown-mode
  :init
  (defun rmd-R-fenced-code-block ()
    "Adds a fenced block for R code in Markdown"
    (interactive)
    (insert "\n```{r}\n\n```\n")
    (forward-line -1)
    (forward-line -1))
  (defun rmd-R-inline-code ()
    "Insert inline R code in Markdown"
    (interactive)
    (insert "`r `")
    (backward-char))
  :config
   (progn
     (add-hook 'markdown-mode-hook
               (lambda ()
         	(imenu-add-menubar-index) ; Add imenu
         	(local-set-key [s-return] 'rmd-R-fenced-code-block) ; C-return to insert a new R chunk
         	(local-set-key [M-return] 'rmd-R-inline-code))))    ; C-S-return to insert inline R code)
   )

;;; Markdown/ESS with polymode
(use-package poly-R
  :ensure t
  )

(use-package poly-markdown
  :ensure t
  )

(use-package polymode
  :ensure t
  :mode
  (("\\.md\\'" . poly-markdown-mode)
   ("\\.Rmd" . poly-markdown+r-mode)
   ("\\.py" . poly-sql-mode))
  :init

  ;; (defun ess-render-rmarkdown ()
  ;;   "Compile R markdown (.Rmd). Should work for any output type."
  ;;   (interactive)
  ;;   ;; Check if attached R-session
  ;;   (condition-case nil
  ;;       (ess-get-process)
  ;;     (error
  ;;      (ess-switch-process)))
  ;;   (let* ((rmd-buf (current-buffer)))
  ;;     (save-excursion
  ;;       (let* ((sprocess (ess-get-process ess-current-process-name))
  ;;              (sbuffer (process-buffer sprocess))
  ;;              (buf-coding (symbol-name buffer-file-coding-system))
  ;;              (buffer-file-name-html (concat (file-name-sans-extension buffer-file-name) ".html"))
  ;;              (R-cmd
  ;;               (format "library(rmarkdown); rmarkdown::render(\"%s\", output_file = \"%s\")"   ;; output_file = 'index.html')"
  ;;                       buffer-file-name buffer-file-name-html)))
  ;;         (message "Running rmarkdown on %s" buffer-file-name)
  ;;         (ess-execute R-cmd 'buffer nil nil)
  ;;         (switch-to-buffer rmd-buf)
  ;;         (ess-show-buffer (buffer-name sbuffer) nil)))))
  
  (defun remove-electric-indent-mode ()
    (electric-indent-local-mode -1))

  (add-hook 'sql-mode-hook 'remove-electric-indent-mode)
  ;;(define-key sql-mode-map (kbd "RET") 'electric-newline-and-maybe-indent)
  ;;(define-key sql-mode-map (kbd "RET") 'ess-noweb-newline)

  (setq-default tab-width 4)
  (setq markdown-enable-math t)

  :config
  (require 'poly-R)		; Load necessary modes
  (require 'poly-markdown)
  )



  
;;; Stan
(use-package stan-mode
  :ensure t)


;;; Magit
(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))

;;; Session manager
(use-package psession
  :ensure t
  :config
  (psession-mode 1)
  (psession-autosave-mode 1))


;; * Move line/region up or down with M-S up/down
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\M-\S-down] 'move-text-down)
(global-set-key [\M-\S-up] 'move-text-up)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * mode line (better bottom line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package doom-modeline
  :ensure t
  :defer t
  ;; :commands (+doom-modeline|init)
  ;; :init (add-hook 'after-init-hook #'+doom-modeline|init)
  :hook (after-init . doom-modeline-mode)
  :config
  (validate-setq
   ;; doom-modeline-hud 1
   ;; recentf-max-saved-items 500
   doom-modeline-project-detection 'projectile
   doom-modeline-buffer-encoding nil
  ;; The maximum displayed length of the branch name of version control.
   doom-modeline-vcs-max-length 12
   doom-modeline-window-width-limit 0.25
   )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Anchored transpose (C-x t on a region, select another region and C-x t again to transpose)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package anchored-transpose
  :bind ("C-x t" . anchored-transpose)
  :config
  (autoload 'anchored-transpose "anchored-transpose" nil t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * google-this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package google-this
  :ensure t
  :bind ("C-x g" . google-this)
  :config
  (google-this-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Shortcut to go to my org dragonfly notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c n") 
                (lambda () (interactive) (find-file "~/Dropbox/dragonfly-notes/dragonfly-notes.org")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))


(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)


(use-package projectile
  :ensure t
)

;; * HELM


;; highlight brackets
(use-package paren
  :ensure t
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'expression)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren t)
  :hook (after-init-hook . show-paren-mode)
  )


;; key to switch between windows
(global-set-key (kbd "C-`") 'other-window)
(global-set-key (kbd "<menu>") 'other-window)
(global-set-key (kbd "<f9>") 'other-window)


(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point)))))
(global-set-key "\M-Z" 'zap-up-to-char)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * i-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rebind to use buffer-menu instead of list-buffers 
;; to keep everything in same buffer
(global-set-key "\C-x\C-b" 'ibuffer)
(setq ibuffer-default-sorting-mode 'filename/process)

;; set sorting column
(setq Buffer-menu-sort-column 4)

(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 30 30 :left :elide)
	      " "
	      (size 6 -1 :right)
	      " "
	      (mode 10 10 :left :elide)
	      " " filename-and-process)
	(mark " "
	      (name 16 -1)
	      " " filename-and-process)))

(setq ibuffer-saved-filter-groups
      '(("home"
	 ("Kaikoura" (filename . "kaikoura"))
	 ("MMRA" (filename . "mmra-2013"))
	 ("Mammals 2013" (filename . "mammals-may"))
	 ("Mammals 2014" (filename . "mammals-2014"))
	 ("Northern royals" (filename . "northern-royal"))
	 ("Oreo" (filename . "oreo"))
	 ("Seabirds 2011" (filename . "seabirds-2011"))
	 ("Seabirds 2013" (filename . "seabirds-2013"))
	 ("Seabirds 2014" (filename . "seabirds-2014"))
	 ("Cryptic mortality" (filename . "doc-cryptic"))
	 ("Bycatch www" (filename . "bycatch.dragonfly"))
	 ("Seabird counts" (or (filename . "abundance/")
			       (filename . "seabird-counts")))
	 ("Maui's dolphins" (filename . "maui"))
	 ("Seabird counts website" (filename . "seabird-counts-website"))
	 ("XBP distribution 2016" (filename . "black-petrel-distribution-2016"))
	 ("XBP distribution" (filename . "black-petrel-distribution"))
	 ("MBIE" (or (filename . "eREAR")
		     (filename . "mbie")
		     (filename . "rear")))
	 ("Overseer" (filename . "nutrient-budget"))
	 ("Antipodean" (filename . "antipodean-albatross"))
	 ("SRA 2012" (filename . "sra-2012"))
	 ("SRA 2014" (filename . "sra-2014"))
	 ("SRA 2016" (filename . "sra-2016"))
	 ("SRA Chile" (filename . "sra-chile"))
	 ("SRA foundations" (filename . "sra-foundations"))
	 ("SRA Kruger" (filename . "sra-kruger"))
	 ("SRA CCSBT" (filename . "ccsbt"))
	 ("Seabird distributions" (filename . "seabird-distributions"))
	 ("Whaleshark" (filename . "whaleshark-interactions"))
	 ("Estimation 2014-15" (filename . "estimation-2014-15"))
	 ("Estimation 2014" (filename . "estimation-2014"))
	 ("Estimation 2015" (filename . "estimation-2015"))
	 ("Estimation 2016-17" (filename . "estimation-2016-17"))
	 ("Estimation 2017-18" (filename . "estimation-2017-18"))
	 ("Ludicio" (filename . "ludicio/"))
	 ("NMS - MfE" (or (filename . "NMS")
			  (filename . "NMS-QC")
			  (filename . "NMS-notes")))
	 ("FIF - MfE" (or (filename . "mfe-water-quality")
			  (filename . "freshwater-triage")))
	 ("sra obs cov" (filename . "sra-observer-coverage/"))
	 ("WHIO benthos" (filename . "whio-benthic-analysis/"))
	 ("NPOA obs optimisation" (filename . "npoa-observer-optimisation"))
	 ("Seabird threats" (filename . "seabird-threat"))
	 ("DOC 5-min bird counts" (filename . "doc-bird"))
	 ("Dropbox" (filename . "Dropbox"))
	 ("Moana paua" (or (filename . "moana")
			   (filename . "BlueAbs")))
	 ("Cadmium" (filename . "cadmium"))
	 ("My tests" (filename . "mytests"))
	 ("SENTIO" (filename . "sentio-leave-calculation"))
	 ("NZ elections" (filename . "nz-elections"))
	 ("emacs-config" (or (filename . ".emacs.d")
			     (filename . "emacs-config")
			     (filename . ".emacs")))
      	 ("SRA global" (or (filename . "sra-southern-hemisphere")
			   (filename . "seabird-risk-assessment")
			   (filename . "sra-example-creation")
			   (filename . "wanderers-at-sea-distribution")
			   (filename . "southern-hemisphere-sra")))
	 ("SRA korea" (filename . "korea-south-africa-sra"))
	 ("EOLC Bill" (filename . "ofc-data-support"))
	 ("YEP" (filename . "yellow-eyed-penguin-overlap"))
	 ("Kakapo" (filename . "kakapo"))
	 ("Paua" (filename . "paua"))
	 ("Fact Benchmark" (filename . "reality-reliability"))
	 ("ES" (filename . "es-ecological-significance"))
	 ("NEFD" (filename . "nefd"))
	 ("GISAID" (or (filename . "tree-server")
		       (filename . "gisaid")))
	 ("Antipodean albatross" (filename . "antipodean-albatross-ipm"))
	 ("SYL" (filename . "syl-"))
	 ("RSI" (filename . "rsi-dashboard"))
	 ("My tests" (or (filename . "my-tests")
			 (filename . "my-musings")
			 (filename . "mytests")))
	 ("Croissant" (filename . "croissant"))
	 ("Fish IBI" (filename . "fish-ibi"))
	 ("DFLY website" (filename . "dragonfly-website"))
	 ("Rec bycatch" (filename . "rec-bycatch"))
)))
(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))

(setq buffer-menu-buffer-font-lock-keywords 
      '(("^....[*]Man .*Man.*" . font-lock-variable-name-face) ;Man page 
	(".*Dired.*" . font-lock-comment-face) ; Dired 
	("^....[*]shell.*" . font-lock-preprocessor-face) ; shell buff 
	(".*[*]scratch[*].*" . font-lock-function-name-face) ; scratch buffer 
	("^....[*].*" . font-lock-string-face) ; "*" named buffers 
	("^..[*].*" . font-lock-constant-face) ; Modified 
	("^.[%].*" . font-lock-keyword-face))) ; Read only

(defun buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
	 (lambda (start end)
	   (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
	 '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(add-hook 'ibuffer-menu-mode-hook 'buffer-menu-custom-font-lock)

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
	   "Open ibuffer with cursor pointed to most recent buffer name"
	   (let ((recent-buffer-name (buffer-name)))
	     ad-do-it
	     (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)


(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

(global-set-key (kbd "C-c l") 'align-regexp)

;; No confirmation on buffer kill
(global-set-key (kbd "C-x k") 'kill-this-buffer)


(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
	 (if (eq last-command 'endless/fill-or-unfill)
	     (progn (setq this-command nil)
		    (point-max))
	   fill-column)))
    (call-interactively #'fill-paragraph)))
(global-set-key [remap fill-paragraph]
		#'endless/fill-or-unfill)


(use-package dumb-jump
  :ensure t
  :config
  (validate-setq
   dumb-jump-prefer-searcher 'ag
   )   
)

(setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
(add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)


;; Change colors for printing with ps-print-*
(ps-extend-face '(default "#000000" nil) 'MERGE)
(ps-extend-face '(font-lock-variable-name-face "#984EA3" nil) 'MERGE)
(ps-extend-face '(font-lock-function-name-face "#084081" nil) 'MERGE)
(ps-extend-face '(rainbow-delimiters-depth-3-face "#FF7F00" nil) 'MERGE)

(put 'erase-buffer 'disabled nil)



;; (defun r-docker-session ()
;;   (interactive)
;;   (setq bpath (file-name-directory buffer-file-name))
;;   (print bpath)
;;   (other-window 1)
;;   (ansi-term "/bin/bash --noprofile --norc")
;;   (rename-buffer "R-term")
;;   (comint-send-string "R-term" (format "cd %s\n" bpath))
;;   (comint-send-string "R-term" "cd $(git rev-parse --show-toplevel)\n")
;;   (comint-send-string "R-term" "make local\n")
;;   (comint-send-string "R-term" "R\n")
;;   (fundamental-mode)
;;   (read-only-mode -1)
;;   (ess-remote nil "R")
;;   (other-window 1)
;;   (ess-switch-process)
;;   ;; (run-with-timer .2 nil 'insert "R-term")
;;   (run-with-timer .3 nil 'execute-kbd-macro (kbd "RET"))
;;   )

(setq ess-r-fetch-ESSR-on-remotes nil)

;; Parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (highlight-parentheses-mode)
    (global-highlight-parentheses-mode))
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
  )

;; (require 'highlight-sexp) ;; manual file
;; (add-hook 'prog-mode-hook 'highlight-sexp-mode)
;; (add-hook 'ess-mode-hook 'highlight-sexp-mode)


(use-package hl-block-mode
  :ensure t
  :config
  (setq hl-block-bracket nil)
  :hook ((prog-mode) . hl-block-mode))


(use-package expand-region
  :ensure t
  :bind ("C-'" . er/expand-region)
  )

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
		 ("C-:" . avy-goto-char)
		 ("C-M-;" . avy-goto-char-2)
		 ("M-g M-g" . avy-goto-line)
		 ("C-0" . avy-pop-mark))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * yas snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet                  ; Snippets (https://www.emacswiki.org/emacs/Yasnippet)
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (snippet-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (conf-mode . yas-minor-mode))
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (validate-setq
   yas-snippet-dirs '("/home/yvan/Dropbox/customisations/yas-snippets")
   )
  )
(add-to-list 'load-path "/home/yvan/Dropbox/customisations/yas-snippets")

(global-set-key [C-tab] 'completion-at-point)

;; Settings for yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package navi-mode
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'ess-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'c++-mode-hook 'outline-minor-mode)
  (add-hook 'lisp-mode-hook 'outline-minor-mode)
  (defvar outline-minor-mode-prefix "\M-#")
  (add-hook 'prog-mode-hook 'outshine-mode)
  )

(defun kill-other-window ()
  "Kill the buffer in the other pane."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  )
(global-set-key "\C-c\C-w" 'kill-other-window)

(global-set-key "\C-z" nil)  ;; prevent emacs from being minimized with C-Z
(global-set-key "\C-x\C-c" nil)  ;; prevent emacs from being killed with C-x C-c



(setq doc-view-continuous t)

(setq max-lisp-eval-depth 10000)

(global-set-key (kbd "<XF86Send>") 'compile)
(global-set-key (kbd "<XF86HomePage>") 'compile)
(global-set-key [f8] (lambda () (interactive) (ess-switch-to-ESS "R")))
(global-set-key [f5] 'compile)

(setq scroll-preserve-screen-position t)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg))
      )))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)


;;; eshell configuration
(use-package eshell
  :ensure t
  :init
  (require 'em-smart)                        ; em-smart: https://www.masteringemacs.org/article/complete-guide-mastering-eshell
  :config
  (validate-setq
   eshell-where-to-jump 'begin
   eshell-review-quick-commands nil
   eshell-smart-space-goes-to-end t))

(global-set-key (kbd "<f4>") 'project-find-file)

(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

(setq visible-bell t)


;; (use-package dired+
;;   :ensure t
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Dired file colours ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired-rainbow
  :ensure t
  :config
(require 'dired-rainbow)
(defconst dired-r-files-extensions
  '("r" "R")
  "Dired R files extensions")
(dired-rainbow-define r "#E31A1C" dired-r-files-extensions)

(defconst dired-program-files-extensions
  '("py" "sql" "sh" "bat" "bug")
  "Dired program files extensions")
(dired-rainbow-define program "#D53E4F" dired-program-files-extensions)

(defconst dired-text-files-extensions
  '("tex" "rnw" "Rnw" "txt" "md" "Rmd" "rmd" "org" "ORG")
  "Dired text files extensions")
(dired-rainbow-define text "#FFEDA0" dired-text-files-extensions)

(defconst dired-gis-files-extensions
  '("dbf" "shp" "qgs" "shx" "prj" "qpj" "kml")
  "Dired gis files extensions")
(dired-rainbow-define gis "#7FBC41" dired-gis-files-extensions)

(defconst dired-data-files-extensions
  '("rdata" "RData" "Rdata" "csv" "CSV" "xls" "xlsx" "XLS" "XLSX" "db")
  "Dired data files extensions")
(dired-rainbow-define data "#FFD92F" dired-data-files-extensions)

(defconst dired-image-files-extensions
  '("png" "pdf" "jpg" "jpeg" "bmp" "PNG" "PDF" "JPG" "JPEG" "BMP" "tif" "tiff" "svg" "SVG")
  "Dired image files extensions")
(dired-rainbow-define image "#DF65B0" dired-image-files-extensions)

(defconst dired-tmp-files-extensions
  '("parsed" "tmp" "test" "r~" "mk~" "log" "R~" "sql~" "old" "rnw~" "tex~" "env~" "bug~")
  "Dired tmp files extensions")
(dired-rainbow-define tmp "#444444" dired-tmp-files-extensions)
)


(setq compilation-scroll-output "first-error")


(use-package multiple-cursors
  :ensure t
  :bind ("C-x m" . mc/mark-all-in-region-regexp)
  )



(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


(use-package rainbow-mode
  :ensure t
  )


(use-package dimmer
  :ensure t
  :config
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-fraction 0.2)
  (dimmer-configure-which-key)
  (dimmer-mode t)
  )



;; Open files
(defun gnome-open-file (filename)
  "gnome-opens the specified file."
  (interactive "fFile to open: ")
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/xdg-open" filename)))

(defun dired-gnome-open-file ()
  "Opens externally the current file in a Dired buffer."
  (interactive)
  (gnome-open-file (dired-get-file-for-visit)))
(add-hook 'dired-mode-hook (lambda () (local-set-key "E" 'dired-gnome-open-file)))

;; (diredp-toggle-find-file-reuse-dir 1)

;; Erase whole line and move to identation
(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)


(global-set-key (kbd "C-x C-j") #'dired-jump)

(use-package git-timemachine
  :ensure t
  )

(defun quote-words (arg)
  "Quote all words"
  (interactive "p")
  (query-replace-regexp "\\(\\<[a-zA-Z0-9._-]+\\>\\)" "\"\\1\"" nil
			(if (use-region-p) (region-beginning))
			(if (use-region-p) (region-end)) nil nil)
  )
(add-hook 'ess-mode-hook 
	  '(lambda ()
	     (local-set-key (kbd "C-c q") 'quote-words)))


(defun ess-abort ()
  (interactive)
  (interrupt-process (ess-get-process)))
(define-key ess-mode-map (kbd "C-c C-q") 'ess-abort)
(define-key inferior-ess-mode-map (kbd "C-c C-q") 'ess-abort)

(defun interupt-job-other-window ()
  "Interupt job in other pane."
  (interactive)
  (other-window 1)
  (comint-interrupt-subjob)
  (other-window -1)
  )
(global-set-key "\C-c\C-a" 'interupt-job-other-window)



;; (setq debug-on-error t)
(setq debug-on-error nil)

(add-hook 'prog-mode-hook 'xref-etags-mode)

(use-package vterm
    :ensure t)

(use-package quarto-mode
  :ensure t)

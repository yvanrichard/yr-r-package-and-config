;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)    ; now you should get a backtrace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/extra-manual")

(require 'package)
(package-initialize)
(setq package-archives
'(("ELPA" . "http://tromey.com/elpa/")
  ("gnu" . "http://elpa.gnu.org/packages/")
  ("melpa" . "http://melpa.org/packages/")
  ("melpa2" . "http://melpa.milkbox.net/packages/")
  ("marmalade" . "http://marmalade-repo.org/packages/")))
;; ("melpa" . "http://stable.melpa.org/packages/")))

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
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output t)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comment-style (quote indent))
 '(completion-ignored-extensions
   (quote
    (".hi" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".r~")))
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(delete-by-moving-to-trash t)
 '(diredp-hide-details-initially-flag nil)
 '(doc-view-resolution 200)
 '(dumb-jump-mode t)
 '(dumb-jump-prefer-searcher (quote ag))
 '(dumb-jump-selector (quote helm))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ess-pdf-viewer-pref "okular")
 '(ess-r-args-electric-paren t)
 '(fci-rule-color "#873b81")
 '(fill-column 110)
 '(fill-prefix nil)
 '(flyspell-default-dictionary "en_GB")
 '(font-latex-fontify-sectioning 1.05)
 '(global-prettify-symbols-mode t)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-ag-use-grep-ignore-list t)
 '(helm-always-two-windows t)
 '(helm-autoresize-mode t)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file)
     (execute-extended-command))))
 '(helm-ff-skip-boring-files t)
 '(helm-mode nil)
 '(helm-mode-fuzzy-match t)
 '(helm-split-window-inside-p t)
 '(helm-swoop-speed-or-color t)
 '(helm-swoop-split-direction (quote split-window-vertically))
 '(helm-swoop-split-with-multiple-windows t)
 '(helm-swoop-use-line-number-face nil)
 '(hl-sexp-background-color "#201520")
 '(ibuffer-filter-group-name-face (quote compilation-info))
 '(ido-ignore-extensions nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(ispell-local-dictionary "en_GB")
 '(magit-repository-directories (quote ("~/dragonfly")))
 '(markdown-command "pandoc --smart -f markdown -t html")
 '(org-babel-R-command "R --vanilla --slave --no-save")
 '(org-babel-load-languages (quote ((latex . t) (R . t) (dot . t) (lisp . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-html-use-infojs t)
 '(org-latex-pdf-process
   (quote
    ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(package-selected-packages
   (quote
    (wakatime-mode auto-dim-other-buffers psession flymd poly-R poly-markdown polymode powerthesaurus latex-preview-pane rainbow-mode helm-descbinds helm-flx eacl help-fns+ helm-projectile docker dockerfile-mode dumb-jump helm-navi navi-mode stan-mode stan-snippets dired-quick-sort ztree expand-region yascroll wrap-region web-mode undo-tree swiper smex smartparens smart-mode-line skewer-mode simplenote2 rainbow-delimiters r-autoyas python-django powerline pg pdf-tools pastels-on-dark-theme org-bullets org multiple-cursors markdown-mode magit latex-pretty-symbols impatient-mode ido-ubiquitous ibuffer-projectile highlight-sexp highlight-indentation helm-swoop helm-make helm-dired-recent-dirs helm-bibtex helm-ag haskell-mode gruber-darker-theme graphviz-dot-mode google-this git-timemachine ggtags flx-ido floobits f3 esup ess-view emacsql elfeed edit-server edbi-minor-mode ebib dired-rainbow dired+ csv-mode company-ess company-auctex color-theme-sanityinc-tomorrow color-theme avy auto-complete-auctex android-mode anchored-transpose ample-theme ag ace-jump-mode Save-visited-files)))
 '(projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")
 '(protect-buffer-bury-p nil)
 '(safe-local-variable-values
   (quote
    ((TeX-master . \./report\.tex)
     (TeX-master . t)
     (TeX-master . "report.tex")
     (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)
     (TeX-master . "report")
     (TeX-master . report\.tex)
     (require-final-newline))))
 '(search-whitespace-regexp "[ \\t\\r\\n]+")
 '(sml/replacer-regexp-list
   (quote
    (("^~/org" ":Org:")
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
     ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))))
 '(sml/shorten-directory t)
 '(tags-table-list (quote ("/home/yvan/Documents/TAGS")))
 '(tramp-default-method "ssh")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
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
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(yank-pop-change-selection t)
 '(yas-also-auto-indent-first-line t))

 ;; '(wakatime-cli-path "/home/yvan/wakatime/wakatime-cli.py")
(defvar outline-minor-mode-prefix "\M-#")

;; (custom-set-variables '(wakatime-api-key "b5cc2771-2403-44ea-821f-46709e5cbb38"))


(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Global
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
					 try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(global-set-key [C-tab] 'completion-at-point)
(global-set-key (kbd "C-\\") 'hippie-expand)

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

(defun kill-other-window ()
  "Kill the buffer in the other pane."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  )
(global-set-key "\C-c\C-w" 'kill-other-window)

;; key to switch between windows
(global-set-key (kbd "s-`") 'other-window)

;; highlight brackets
(require 'paren)
(show-paren-mode 1)

;; colour highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(global-set-key "\C-z" nil)  ;; prevent emacs from being minimized with C-Z
(global-set-key "\C-x\C-c" nil)  ;; prevent emacs from being killed with C-x C-c

(global-set-key (kbd "C-<") 'previous-buffer)                                  
(global-set-key (kbd "C->") 'next-buffer)                                 

(defun back-to-indentation-or-beginning () (interactive)
   (if (= (point) (progn (back-to-indentation) (point)))
       (beginning-of-line)))
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

;; For completion in ESS (keybinding conflit with TAB)
;;(global-set-key (kbd "C-'") 'completion-at-point)


;; Search in multiple buffers (first argument is regexp on buffer names)
;; (global-set-key "\C-c\M-m" 'multi-occur-in-matching-buffers)
(global-set-key "\C-c\M-m" 'helm-ag-buffers)

(setq doc-view-continuous t)

;; ;; use autofill on text modes
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq max-lisp-eval-depth 10000)

;; (require 'pabbrev)

(global-set-key (kbd "<XF86Send>") 'compile)
(global-set-key (kbd "<XF86HomePage>") 'compile)
(global-set-key [f8] (lambda () (interactive) (ess-switch-to-ESS "R")))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Colours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#202020" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "SRC" :family "Hack"))))
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
 '(ess-numbers-face ((t (:foreground "#FFFF88" :weight normal))))
 '(eww-form-submit ((t (:background "#444444" :foreground "#EEEEEE" :box (:line-width 2 :style released-button)))))
 '(eww-form-textarea ((t (:background "#C0C0C0" :foreground "black" :box 1))))
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
 '(font-lock-constant-face ((t (:inherit default :foreground "#55AA55"))))
 '(font-lock-function-name-face ((t (:foreground "light blue"))))
 '(font-lock-keyword-face ((t (:foreground "#88DD88"))))
 '(font-lock-string-face ((t (:foreground "DarkSeaGreen2"))))
 '(font-lock-type-face ((t (:foreground "yellow green"))))
 '(font-lock-warning-face ((t (:background "yellow" :foreground "red" :weight semi-bold))))
 '(helm-buffer-file ((t nil)))
 '(helm-ff-directory ((t (:inherit diredp-dir-priv))))
 '(helm-selection ((t (:background "#550000" :distant-foreground "black" :weight bold))))
 '(helm-swoop-target-line-block-face ((t (:background "#444400"))))
 '(helm-swoop-target-line-face ((t (:background "#444444"))))
 '(hi-yellow ((t (:background "yellow1" :foreground "red"))))
 '(highlight ((t (:background "#552222"))))
 '(highlight-indentation-face ((t (:inherit fringe :background "gray30"))))
 '(italic ((t (:height 0.7))))
 '(link ((t (:foreground "#CCDDFF" :underline "#110011"))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :foreground "spring green" :height 1.0))))
 '(match ((t (:background "#224477"))))
 '(org-block ((t (:inherit shadow :foreground "#AAFFAA"))))
 '(org-code ((t (:inherit shadow :foreground "#AAFFAA"))))
 '(org-date ((t (:foreground "#AACCFF" :underline t))))
 '(org-level-1 ((t (:foreground "#FFF7BC"))))
 '(org-level-2 ((t (:foreground "#FCBBA1"))))
 '(org-level-3 ((t (:foreground "#C6DBEF"))))
 '(org-level-4 ((t (:foreground "#CCEBC5"))))
 '(org-special-keyword ((t (:foreground "#66FFFF"))))
 '(outline-1 ((t (:background "#181818" :foreground "#FF2A17" :box (:line-width 1 :color "grey35") :slant italic :weight normal))))
 '(outline-2 ((t (:background "#191919" :foreground "#FF794A" :box (:line-width 1 :color "grey30") :slant italic))))
 '(outline-3 ((t (:background "#191919" :foreground "#FFB67D" :box (:line-width 1 :color "grey20") :slant italic))))
 '(outline-4 ((t (:inherit font-lock-comment-face :background "#191919" :foreground "RosyBrown1" :slant italic))))
 '(outline-5 ((t (:background "#191919" :foreground "#FFF1CC" :slant italic))))
 '(popup-scroll-bar-foreground-face ((t (:background "red"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ff7777"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#e69333"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffff88"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#66ff66"))))
 '(region ((t (:background "grey20"))))
 '(underline ((t (:underline "#666666"))))
 '(widget-button ((t (:foreground "deep sky blue" :weight bold)))))


(global-wakatime-mode)

;; ;;;;;;;;;;;;;;;;
;; ;; * eshell   ;;
;; ;;;;;;;;;;;;;;;;
;; (setenv "PAGER" "cat")
;; (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Latex, Sweave, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq reftex-cite-format 'natbib)
;; (setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(setq latex-run-command "xelatex")

;; (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
;; (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
;; (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
;; (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)


;; (add-hook 'LaTeX-mode-hook
;; 	  '(lambda()
;; 	     (local-set-key [(tab)] 'hippie-expand)))
;; (add-hook 'latex-mode-hook
;; 	  '(lambda()
;; 	     (local-set-key [(tab)] 'hippie-expand)))


;;;;;;;;;;;;;;;;;;;
;; * Spell check ;;
;;;;;;;;;;;;;;;;;;;

;; enable on-the-fly spell checking
;; (add-hook 'emacs-startup-hook
;;           (lambda()
;;             (add-hook 'text-mode-hook
;;                       (lambda ()
;;                         (flyspell-mode 1)))
;;             ;; prevent flyspell from finding mistakes in the code
;;             (add-hook 'prog-mode-hook
;;                       (lambda ()
;;                         ;; `ispell-comments-and-strings'
;;                         (flyspell-prog-mode)))))

;; ;; ispell should not check code blocks in org mode
;; (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
;; (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
;; (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
;; (add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
;; (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))

;; (add-to-list 'ispell-skip-region-alist '("^<<" . "@$"))


;; (require 'latex-frame-mode)

;; (setq reftex-default-bibliography '("/home/yvan/dragonfly/bibliography/mfish.bib"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * r-markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * ido-mode (autocompletion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ido-mode t)
;; (setq ido-enable-flex-matching t)

;; ;; Flex ido
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (ido-ubiquitous-mode 1)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; ;; (setq ido-use-faces nil)
;; (setq org-completion-use-ido t)
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode 1)

;; (ivy-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * utils for finding non-ascii characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper functions to find non-ascii characters
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))



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
	 ("Seabird counts" (filename . "abundance/"))
	 ("Maui's dolphins" (filename . "maui"))
	 ("Seabird counts website" (filename . "seabird-counts-website"))
	 ("XBP distribution 2016" (filename . "black-petrel-distribution-2016"))
	 ("XBP distribution" (filename . "black-petrel-distribution"))
	 ("MBIE" (or (filename . "eREAR")
		     (filename . "mbie")
		     (filename . "rear")))
	 ("Overseer" (filename . "nutrient-budget"))
	 ("SRA 2012" (filename . "sra-2012"))
	 ("SRA 2014" (filename . "sra-2014"))
	 ("SRA 2016" (filename . "sra-2016"))
	 ("SRA Chile" (filename . "sra-chile"))
	 ("SRA foundations" (filename . "sra-foundations"))
	 ("SRA Kruger" (filename . "sra-kruger"))
	 ("Whaleshark" (filename . "whaleshark-interactions"))
	 ("Estimation 2014-15" (filename . "estimation-2014-15"))
	 ("Estimation 2014" (filename . "estimation-2014"))
	 ("Estimation 2015" (filename . "estimation-2015"))
	 ("Estimation 2016-17" (filename . "estimation-2016-17"))
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * ESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; (require 'ess-jags-d)
;; ;; (autoload 'ess-jags-mode "ess-jags-mode"
;; ;;    "Major mode for editing JAGS files" t)
;; (add-to-list 'auto-mode-alist '("\\.jags\\'" . ess-jags-mode))


;; ;; (require 'ess-eldoc) ;to show function arguments while you are typing them
;; (setq ess-use-auto-complete 'script-only)
;; ;; (require 'ess-site)

;; (setq ess-eval-visibly-p t) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start

;; ;; (setq ess-local-process-name "R")
;; (setq ansi-color-for-comint-mode 'filter)
;; (setq comint-scroll-to-bottom-on-input t)
;; (setq comint-scroll-to-bottom-on-output t)
;; (setq comint-move-point-for-output t)

;; (defun my-ess-start-R ()
;;   (interactive)
;;   (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
;;       (progn
;; 	(delete-other-windows)
;; 	(setq w1 (selected-window))
;; 	(setq w1name (buffer-name))
;; 	(setq w2 (split-window w1))
;; 	(R)
;; 	(set-window-buffer w2 "*R*")
;; 	(set-window-buffer w1 w1name))))
;; (defun my-ess-eval ()
;;   (interactive)
;;   (my-ess-start-R)
;;   (if (and transient-mark-mode mark-active)
;;       (call-interactively 'ess-eval-region)
;;     (call-interactively 'ess-eval-line-and-step)))

;; (add-hook 'ess-mode-hook
;; 	  '(lambda()
;; 	     ;; (local-set-key [(shift return)] 'my-ess-eval)
;; 	     (local-set-key [(backtab)] 'ess-indent-or-complete)))
;; (add-hook 'inferior-ess-mode-hook
;; 	  '(lambda()
;; 	     (local-set-key [C-up] 'comint-previous-input)
;; 	     (local-set-key [C-down] 'comint-next-input)))

;; (ess-toggle-underscore nil)
;; (setq ess-S-assign-key (kbd "C-="))
;; (ess-toggle-S-assign-key t)
;; (ess-auto-newline t)
;; (ess-toggle-S-assign nil)
;; (ess-toggle-S-assign nil)


(defun comint-interrupt-subjob-other ()
  "Interrupt process in the other pane."
  (interactive)
  (other-window 1)
  (comint-interrupt-subjob)
  (other-window -1)
  )
(global-set-key "\C-c\C-q" 'comint-interrupt-subjob-other)

(defun interupt-job-other-window ()
  "Interupt job in other pane."
  (interactive)
  (other-window 1)
  (comint-interrupt-subjob)
  (other-window -1)
  )
(global-set-key "\C-c\C-a" 'interupt-job-other-window)

(setq ess-indent-level 4)

;; (add-to-list 'auto-mode-alist '("\\.Rnw" . Rnw-mode))
;; (add-to-list 'auto-mode-alist '("\\.rnw" . Rnw-mode))

;; (add-to-list 'auto-mode-alist '("\\.1" . R-mode))
;; (add-to-list 'auto-mode-alist '("makefile\\.1" . makefile-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'anything-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-src-fontify-natively t)
(setq org-support-shift-select t)

(setq org-ellipsis "⤵")

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)))

;; (require 'org-bullets)
;; (setq org-bullets-bullet-list
;;       '("◉" "◎" "⚫" "○" "►" "◇"))
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * uniquify (change file<2> to file/fold)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify) 
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Automatically save and restore sessions
;; (setq desktop-dirname             "~/.emacs.d/desktop/"
;;       desktop-base-file-name      "emacs.desktop"
;;       desktop-base-lock-name      "desktoplock"
;;       desktop-path                (list desktop-dirname)
;;       desktop-save                t
;;       )
;; (desktop-save-mode 1)
;; (add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
;; ;; (desktop-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Copy buffer file path to clipboad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "C-c P") 'my-put-file-name-on-clipboard)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Wrap region  (https://github.com/rejeep/wrap-region.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (wrap-region-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Ace jump mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)

(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-M-;") 'avy-goto-char-2)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-0") 'avy-pop-mark)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * expand-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'expand-region) 
(global-set-key (kbd "C-'") 'er/expand-region)

;; (defun er/mark-r-assignment ()
;;   "Mark R assignment such as 'var <- something'."
;;   (interactive)
;;   (when (or (looking-at "\\(\\s_\\|\\sw\\)* <- ")
;;             (er/looking-back-exact "<-"))
;;     (search-backward "<-")
;;     (back-to-indentation)
;;     (set-mark (point))
;;     (search-forward "<-")
;;     (forward-sexp 1)
;;     (exchange-point-and-mark)))

;; (defun er/add-r-mode-expansions ()
;;   "Adds R-specific expansions for buffers in html-mode"
;;   (set (make-local-variable 'er/try-expand-list) (append
;;                                                   er/try-expand-list
;;                                                   '(er/mark-r-assignment))))

;; (er/enable-mode-expansions 'r-mode 'er/add-r-mode-expansions)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * yas snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq yas-snippet-dirs
      '("/home/yvan/Dropbox/customisations/yas-snippets"))
(add-to-list 'load-path "/home/yvan/Dropbox/customisations/yas-snippets")

(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'powerline)
;; (powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * smart-mode-line (better bottom line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sml/setup)
(sml/apply-theme 'dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Highlight sexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-highlight-sexp-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Scroll bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-yascroll-bar-mode 1)
;; (scroll-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Tramp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Abbreviations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-abbrev-table 'global-abbrev-table '(

    ;; normal english words
    ("0pop" "population")
    ("0vul" "vulnerability")
    ("0sp"  "species")
    ("0xwm" "New Zealand white-capped albatross")
    ("0xcm" "Campbell black-browed albatross")
    ("0xal" "albatross")
    ("0xan" "Antipodean albatross")
    ("0xau" "Gibson's albatross")
    ("0xbm" "southern Buller's albatross")
    ("0xbp" "black petrel")
    ("0xci" "Chatham Island albatross")
    ("0xfs" "flesh-footed shearwater")
    ("0xnb" "northern Buller's albatross")
    ("0xnr" "northern royal albatross")
    ("0xra" "southern royal albatross")
    ("0xsa" "Salvin's albatross")
    ("0xsh" "sooty shearwater")
    ("0xwc" "white-chinned petrel")
    ("0xyp" "yellow-eyed penguin")
    ("0pbr" "Potential Biological Removal")
    ("0apf" "Annual Potential Fatalities")
    ("0nz"  "New Zealand")
    ("0ssp" "subspecies")
    ("0bll" "bottom longline")
    ("0sll" "surface longline")
    ("0sra2t" "\citet{richard_risk_2013}")
    ("0sra2p" "\citep{richard_risk_2013}")
    ("0sra" "seabird risk assessment")    
    ))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Anchored transpose (C-x t on a region, select another region and C-x t again to transpose)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x t") 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * google-this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x g") 'google-this)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Company (an auto-complete package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'global-company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)



;; ;; Increase quality of viewed pdfs
;; (defcustom doc-view-ghostscript-options
;;           '("-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4"
;;             "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET"
;;             "-r300")
;;           "A list of options to give to ghostview."
;;           :type '(sexp)
;;           :group 'doc-view)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;    buffer-timer
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'buffer-timer)

;; ;; ; Example list of titles and regexps to group by.  This
;; ;; (setq buffer-timer-regexp-master-list
;; ;;   '(
;; ;;     ("idle" . 
;; ;;      (("generic" .			  "^\\*idle\\*")
;; ;;       ("generic2" .			  "^\\*idle-2\\*")
;; ;;       ("minibuf" .                        "^ \\*Minibuf-.*")))
;; ;;     ("customizations" .                   "\\.emacs")
;; ;;     ("work" .
;; ;;      (("my R pkg" .                       "yr-r-package")
;; ;;       ("seabird counts" .
;; ;;        (("Main project" .                 "abundance")
;; ;; 	("Web site" .                     "seabird-counts-website")))
;; ;;       ("SRA 2012" .
;; ;;        (("analysis" .                     "sra-2012/analysis")
;; ;; 	("report" .                       "sra-2012/report")))
;; ;;       ("Encounter kaikoura" .
;; ;;        (("data" .                          "encounter-kaikoura/data")
;; ;; 	("analysis" .                      "encounter-kaikoura/analysis")
;; ;; 	("plots" .                         "encounter-kaikoura/plots")
;; ;; 	("report" .                        "encounter-kaikoura/report")))
;; ;;       ("XNR Taiaroa" .
;; ;;        (("data" .                          "northern-royal-albatross-taiaroa/data")
;; ;; 	("analysis" .                      "northern-royal-albatross-taiaroa/analysis")
;; ;; 	("plots" .                         "northern-royal-albatross-taiaroa/plots")
;; ;; 	("report" .                        "northern-royal-albatross-taiaroa/report")))
;; ;;       ("R terminal" .                     "^\\*R\\*$")
;; ;;       ("Dragonfly others" .               "/dragonfly/")))
;; ;;     )
;; ;; )



;; ;; Macro to reformat curly brackets (to fix my old style)
;; (fset 'reparens
;;    [?\S-\C-\M-s ?^ ?\[ ?\[ ?: ?b ?l ?a ?n ?k ?: ?\] ?\] ?* ?\{ return ?\C-b ?\M-^ ?\C-\M-q ?\C-e])

;; ;; Macro to insert a space on both sides of equal signs
;; (fset 'eqspacing
;;    [?\C-\M-% ?\\ ?\( ?\[ ?^ ?\[ ?: ?b ?l ?a ?n ?k ?: ?\] ?\] ?+ ?\\ ?\) ?= ?\\ ?\( ?\[ ?^ ?\[ ?: ?b ?l ?a ?n ?k ?: ?\] ?\] S-backspace ?\] ?+ ?\\ ?\) left left left left left left left left left left left left left left left left left left left left left delete right right right right right right right right right right right right right right right right right delete end return ?\\ ?1 ?  ?= ?  ?\\ ?2 return])

(defun eqspacing (arg)
  "Add space around `=' when there is none"
  (interactive "p")
  (beginning-of-buffer)
  (replace-regexp "\([^[:blank:]=:!]\)=" "\1 =")
  (replace-regexp "=\([^[:blank:]=:!]\)" "= \2"))


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

(diredp-toggle-find-file-reuse-dir 1)

;; Erase whole line and move to identation
(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)



;; ;; Change order of positions with recentering cursor with C+l
;; (setq recenter-positions '(top middle bottom))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Pretty mode - Display symbols as symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; (global-pretty-mode 1)
;; (add-hook 'ess-mode-hook
;;             (lambda ()
;;               (push '("function" . ?ƒ) prettify-symbols-alist)
;; 	      (push '("sum" . ?Σ) prettify-symbols-alist)
;; 	      (push '("<=" . ?≤) prettify-symbols-alist)
;; 	      (push '(">=" . ?≥) prettify-symbols-alist)
;; 	      (push '("sqrt" . ?√) prettify-symbols-alist)
;; 	      (push '("..." . ?…) prettify-symbols-alist)
;; 	      (push '("pi" . ?𝜋) prettify-symbols-alist)
;; 	      (push '("alpha" . ?𝛼) prettify-symbols-alist)
;; 	      (push '("beta" . ?𝛽) prettify-symbols-alist)
;; 	      (push '("gamma" . ?ɣ) prettify-symbols-alist)
;; 	      (push '("mu" . ?μ) prettify-symbols-alist)
;; 	      (push '("theta" . ?θ) prettify-symbols-alist)
;; 	      (push '("eps" . ?ε) prettify-symbols-alist)
;; 	      (push '("!=" . ?≠) prettify-symbols-alist)
;; 	      (push '("<-" . ?⇽) prettify-symbols-alist)
;; 	      (push '("<<-" . ?⇐) prettify-symbols-alist)))


;; ;; specify font for all unicode characters
;; ;; (when (member "Symbola" (font-family-list))
;; ;; (set-fontset-font t 'unicode "Ubuntu Mono" nil 'prepend)
;; (set-fontset-font "fontset-default" nil 
;;                   (font-spec :size 20 :name "Ubuntu Mono"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Wrap region - Select region then " to enclose it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(wrap-region-global-mode 1)


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'eshell-mode-hook 'ansi-color-for-comint-mode-on)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Shortcut to go to my org dragonfly notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c n") 
                (lambda () (interactive) (find-file "~/Dropbox/dragonfly-notes/dragonfly-notes.org")))



;;;;;;;;;;;;;;;;;;;;;;;;
;; * Multiple cursors ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x m") 'mc/mark-all-in-region-regexp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Replace open-line (C-o) to keep identation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun open-previous-line (arg)
;;   "Open a new line before the current one. 
;;      See also `newline-and-indent'."
;;   (interactive "p")
;;   (beginning-of-line)
;;   (open-line arg)
;;   (when newline-and-indent
;;     (indent-according-to-mode)))
;; (global-set-key (kbd "C-S-o") 'open-previous-line)

;; (defun open-next-line (arg)
;;   "Move to the next line and then opens a line.
;;     See also `newline-and-indent'."
;;   (interactive "p")
;;   (push-mark)
;;   (end-of-line)
;;   (open-line arg)
;;   (next-line 1)
;;   (when newline-and-indent
;;     (indent-according-to-mode))
;;   (exchange-point-and-mark))
;; (global-set-key (kbd "C-o") 'open-next-line)

;; ;; ;; Autoindent open-*-lines
;; ;; (defvar newline-and-indent t
;; ;;   "Modify the behavior of the open-*-line functions to cause them to autoindent.")



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * rtags and visit file
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun rtags (arg)
;;   "Execute R rtags function and visit tag file."
;;   (interactive "p")
;;   (shell-command "Rscript -e \"rtags(ofile='TAGS', recursive=T)\"")
;;   (visit-tags-table "TAGS"))
;; ;; (add-hook 'ess-mode-hook 
;; ;; 	  '(lambda ()
;; ;; 	     (local-set-key (kbd "C-c t") 'rtags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * evaluate R function arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ess-eval-function-args (arg)
  "Evaluate default arguments of R function."
  (interactive "p")
  (push-mark)
  (ess-goto-beginning-of-function-or-para)
  (search-forward "(")
  (backward-char)
  (push-mark)
  (forward-sexp)
  (kill-ring-save (region-beginning) (region-end))
  (with-temp-buffer
    (setq ess-dialect "R")
    (yank)
    (end-of-buffer)
    (search-backward ")")
    (delete-char 1)
    (goto-char 1)
    (search-forward "(")
    (backward-char)
    (delete-char 1)
    (goto-char 1)
    (while (search-forward "," nil t) (replace-match "\n" t nil))
    (goto-char 1)
    (while (search-forward-regexp "^[:blank:]*[^=]+$" nil t) (replace-match "" nil t))
    (ess-eval-buffer-and-go nil)
    (buffer-string)
    )
  (pop-global-mark)
  )
(add-hook 'ess-mode-hook 
	  '(lambda ()
	     (local-set-key (kbd "C-c f") 'ess-eval-function-args)))

(defun ess-for-to-sapply (arg)
  "Change 'for' structure to 'sapply'."
  (interactive "p")
  (push-mark)
  (search-backward-regexp "^[[:blank:]]*for")
  (search-forward-regexp "for *" nil t)
  (replace-match "s <- sapply" nil t)
  (forward-char 1)
  (er/expand-region 1)
  (kill-region (region-beginning) (region-end))
  (search-forward-regexp " *in *" nil t)
  (replace-match "" nil t)
  (backward-char)
  (forward-sexp)
  (backward-char)
  (insert ", function(")
  (yank)
  (insert ")")
  (delete-char 1)
  (ess-indent-exp)
  (forward-sexp)
  (insert ")")
  (pop-global-mark)
  )

;; (add-hook 'ess-mode-hook (lambda () (setq ess-arg-function-offset nil)))
;; (add-hook 'ess-mode-hook (lambda () (setq ess-first-continued-statement-offset 2)))
;; (add-hook 'ess-mode-hook (lambda () (setq ess-continued-statement-offset 0)))

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


;; (setq magit-last-seen-setup-instructions "1.4.0")
(setenv "EDITOR" "emacsclient")

(global-set-key (kbd "C-c m") 'magit-status)

;; ;; (make-face 'font-lock-punctuation-face) ;; Create a new face
;; ;; (set-face-foreground 'font-lock-punctuation-face "red") ;; Set the colour
;; (defun custom-punc()
;;   "adds a few special keywords for c and c++ modes"
;;   (font-lock-add-keywords nil
;;    '(
;;      ("\\.;:," . 'font-latex-warning-face )
;;      )
;;    )
;;   )
;; (add-hook 'LaTeX-mode-hook 'custom-punc)
;; (add-hook 'latex-mode-hook 'custom-punc)
;; (add-hook 'tex-mode-hook 'custom-punc)
;; (add-hook 'Rnw-mode-hook 'custom-punc)

;; (add-to-list 'TeX-symbol-list ";")

(setq compilation-scroll-output "first-error")

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;   (font-lock-add-keywords nil         
;;     '((":,.;" 0 font-lock-warning-face)))
;; ))
;; (add-hook 'latex-mode-hook (lambda ()
;;   (font-lock-add-keywords nil         
;;     '((":,.;" 0 font-lock-warning-face)))
;; ))
;; (add-hook 'tex-mode-hook (lambda ()
;;   (font-lock-add-keywords nil         
;;     '((":,.;" 0 font-lock-warning-face)))
;; ))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Dired file colours ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; (defconst dired-dot-files-extensions
;;   '"\\..*"
;;   "Dired dot files extensions")
;; (dired-rainbow-define dot (:inherit default
;; 				    :italic t
;; 				    :foreground "#666666") dired-dot-files-extensions)

(require 'dired+)
;; (setq dired-dwim-target t)



(add-to-list 'auto-mode-alist '("\\.env\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.env0\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("makefile\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("png\\'" . image-mode))


;; (make-face 'font-lock-r-df-face) ;; Create a new face
;; (set-face-foreground 'font-lock-r-df-face "#BBBB99") ;; Set the colour
;; (make-face 'font-lock-r-dollar-face) ;; Create a new face
;; (set-face-foreground 'font-lock-r-dollar-face "#666666") ;; Set the colour
;; (make-face 'font-lock-r-special-face) ;; Create a new face
;; (set-face-foreground 'font-lock-r-special-face "#444444") ;; Set the colour
;; (add-hook 'ess-mode-hook (lambda ()
;; 			   (font-lock-add-keywords nil
;; 						   '(("\\([a-zA-Z0-9_.]+\\)\\$" 1  'font-lock-r-df-face prepend)
;; 						     ("\\([a-zA-Z0-9_.]+\\)\\[\\[" 1  'font-lock-r-df-face prepend)
;; 						     ("\\(\\$\\)" 1  'font-lock-r-dollar-face prepend)
;; 						     ("\\(,\\)" 1  'font-lock-r-dollar-face prepend)
;; 						     ;; ("\\(\\[\\[\\)" 1  'font-lock-r-dollar-face prepend)
;; 						     ("\\(\\[\\)\\[" 1  'font-lock-r-dollar-face prepend)
;; 						     ("\\[\\(\\[\\)" 1  'font-lock-r-special-face prepend)
;; 						     ("\\(\\]\\)\\]" 1  'font-lock-r-special-face prepend)
;; 						     ("\\]\\(\\]\\)" 1  'font-lock-r-dollar-face prepend)
;; 						     ("\\([0-9.]+e[\\-0-9]+\\)" 1  'ess-numbers-face prepend)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Code folding & indentation & formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
;; (global-rainbow-delimiters-mode)

;; Highlight region between parentheses
;; (require 'paren)
;; (set-face-background 'show-paren-match-face "#696969")
;; (set-face-foreground 'show-paren-match-face "#def")
;; (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
;; (setq show-paren-delay 0)
;; (show-paren-mode 1)

;; (require 'highlight-indentation)
;; ;; (add-hook 'ess-mode-hook 'highlight-indentation-mode) 
;; (add-hook 'lisp-mode-hook 'highlight-indentation-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "C-c h") 'helm-mini)

(require 'helm)
(require 'helm-config)
(helm-mode 1)
;; (global-set-key (kbd "C-c g") 'helm-do-grep)
;; (eval-after-load 'helm-grep
;;   '(setq helm-grep-default-command helm-grep-default-recurse-command))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
;; (global-set-key (kbd "C-:") 'ac-complete-with-helm)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)


;; To search ignoring whitespaces
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace t)
(setq search-whitespace-regexp "[ \t\r\n]+")

;; Do not use word at point by default for helm-swoop
(setq helm-swoop-pre-input-function (lambda () ""))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * projectile
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; from http://endlessparentheses.com/improving-projectile-with-extra-commands.html
;; (projectile-global-mode)
;; (setq projectile-keymap-prefix (kbd "C-x p"))
;; (setq projectile-switch-project-action
;;       #'projectile-commander)
;; (def-projectile-commander-method ?s
;;   "Open a *shell* buffer for the project."
;;   (shell (get-buffer-create
;;           (format "*shell %s*"
;;                   (projectile-project-name)))))

;; (def-projectile-commander-method ?c
;;   "Run `compile' in the project."
;;   (call-interactively #'compile))
;; (def-projectile-commander-method ?\C-?
;;   "Go back to project selection."
;;   (projectile-switch-project))

;; (setq projectile-enable-caching t)
;; (setq projectile-completion-system 'helm)
;; ;; (helm-projectile-on)



;; (global-set-key (kbd "C-.") 'imenu-anywhere)
(global-set-key (kbd "C-S-s") 'helm-occur)
(global-set-key (kbd "C-s") 'isearch-forward)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Popping marks faster ;;
;; ;;;;;;;==;;;;;;;;;;;;;;;;;;;
;; ;; From: http://endlessparentheses.com/faster-pop-to-mark-command.html
;; ;; When popping the mark, continue popping until the cursor
;; ;; actually moves
;; (defadvice pop-to-mark-command (around ensure-new-position activate)
;;   (let ((p (point)))
;;     (dotimes (i 10)
;;       (when (= p (point)) ad-do-it))))
;; (setq set-mark-command-repeat-pop t)



(setq visible-bell t)





;; (defun rmarkdown-to-html ()
;;   (interactive)
;;   "Run knitr::knit2html on the current file"
;;   "https://gist.github.com/kohske/9128031"
;;   (shell-command
;;    (format "Rscript -e \"rmarkdown::render('%s')\""
;; 	   (shell-quote-argument (buffer-file-name)))))
 
;; ;; do this in R process
;; ;; library (rmarkdown); render ("file_name.Rmd")
 
;; (defun ess-rmarkdown ()
;;   (interactive)
;;   "Compile R markdown (.Rmd). Should work for any output type."
;;   "http://roughtheory.com/posts/ess-rmarkdown.html"
;;   ; Check if attached R-session
;;   (condition-case nil
;;       (ess-get-process)
;;     (error
;;      (ess-switch-process)))
;;   (let* ((rmd-buf (current-buffer)))
;;     (save-excursion
;;       (let* ((sprocess (ess-get-process ess-current-process-name))
;;          (sbuffer (process-buffer sprocess))
;;          (buf-coding (symbol-name buffer-file-coding-system))
;;          (R-cmd
;;           (format "library (rmarkdown); rmarkdown::render (\"%s\")"
;;               buffer-file-name)))
;;     (message "Running rmarkdown on %s" buffer-file-name)
;;     (ess-execute R-cmd 'buffer nil nil)
;;     (switch-to-buffer rmd-buf)
;;     (ess-show-buffer (buffer-name sbuffer) nil)))))
 
;; (define-key polymode-mode-map "\M-ns" 'ess-rmarkdown)


;; (setq elfeed-feeds
;;       '("http://avxhome.in/ebooks/science_books/rss.xml"
;; 	"http://avxhome.in/ebooks/programming_development/rss.xml"
;; 	"http://avxhome.in/ebooks/software/rss.xml"
;; 	"http://avxhome.in/ebooks/music/rss.xml"
;; 	"http://avxhome.in/ebooks/animals/rss.xml"))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Auto-correct with C-x C-i ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; add correction automatically to abbrev list for future automatic correction
;; ;; From http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

;; (define-key ctl-x-map "\C-i"
;;   #'endless/ispell-word-then-abbrev)

;; (defun endless/ispell-word-then-abbrev (p)
;;   "Call `ispell-word', then create an abbrev for it.
;; With prefix P, create local abbrev. Otherwise it will
;; be global.
;; If there's nothing wrong with the word at point, keep
;; looking for a typo until the beginning of buffer. You can
;; skip typos you don't want to fix with `SPC', and you can
;; abort completely with `C-g'."
;;   (interactive "P")
;;   (let (bef aft)
;;     (save-excursion
;;       (while (if (setq bef (thing-at-point 'word))
;;                  ;; Word was corrected or used quit.
;;                  (if (ispell-word nil 'quiet)
;;                      nil ; End the loop.
;;                    ;; Also end if we reach `bob'.
;;                    (not (bobp)))
;;                ;; If there's no word at point, keep looking
;;                ;; until `bob'.
;;                (not (bobp)))
;;         (backward-word))
;;       (setq aft (thing-at-point 'word)))
;;     (if (and aft bef (not (equal aft bef)))
;;         (let ((aft (downcase aft))
;;               (bef (downcase bef)))
;;           (define-abbrev
;;             (if p local-abbrev-table global-abbrev-table)
;;             bef aft)
;;           (message "\"%s\" now expands to \"%s\" %sally"
;;                    bef aft (if p "loc" "glob")))
;;       (user-error "No typo at or before point"))))

;; (setq save-abbrevs 'silently)
;; (setq-default abbrev-mode t)



;; (require 'ess-view)


(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)


;;;;;;;;;;;;
;; * TAGS ;;
;;;;;;;;;;;;

;; (setq path-to-ctags "/usr/bin/ctags-exuberant")
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command
;;    (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
;;   )


(global-set-key (kbd "C-c l") 'align-regexp)


;; ANSI-colors in the compilation buffer output
;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
	  'endless/colorize-compilation)

;; (setq x-select-enable-primary nil)
;; (setq x-select-enable-clipboard t)

;; (setq x-selection-timeout 10)


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


;;; Sending input to compilation buffer
;; http://endlessparentheses.com/provide-input-to-the-compilation-buffer.html

;; (defun endless/send-input (input &optional nl)
;;   "Send INPUT to the current process.
;; Interactively also sends a terminating newline."
;;   (interactive "MInput: \nd")
;;   (let ((string (concat input (if nl "\n"))))
;;     ;; This is just for visual feedback.
;;     (let ((inhibit-read-only t))
;;       (insert-before-markers string))
;;     ;; This is the important part.
;;     (process-send-string
;;      (get-buffer-process (current-buffer))
;;      string)))

;; (defun endless/send-self ()
;;   "Send the pressed key to the current process."
;;   (interactive)
;;   (endless/send-input
;;    (apply #'string
;;           (append (this-command-keys-vector) nil))))

;; (define-key compilation-mode-map (kbd "C-c i")
;;   #'endless/send-input)

;; (dolist (key '("\C-d" "\C-j" "y" "n"))
;;   (define-key compilation-mode-map key
;;     #'endless/send-self))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; * Polymode for RMarkdown ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; Markdown mode
;; (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
;; (setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.ronn?" . markdown-mode) auto-mode-alist))

;; ;;; Polymode
;; ;; (setq load-path (append '("/home/sbonner/.emacs.d/polymode/" "/home/sbonner/.emacs.d/polymode/modes") load-path))

;; (require 'poly-R)
;; (require 'poly-markdown)
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.rmd" . poly-markdown+r-mode))


;; Insert date, time, or both
(defun jcs-datetime (arg)
  "Without argument: insert date as yyyy-mm-dd
With C-u: insert time
With C-u C-u: insert date and time"
  (interactive "P")
  (cond ((equal arg '(4)) (insert (format-time-string "%T")))
        ((equal arg '(16)) (insert (format-time-string "%Y-%m-%d %T")))
        (t (insert (format-time-string "%Y-%m-%d")))))

(global-set-key (kbd "C-c D") 'jcs-datetime)




(defun my/dired-untracked (dir)
  (interactive "DUntracked in directory: ")
  (cd dir)
  (switch-to-buffer (get-buffer-create "*untracked*"))
  (shell-command "git ls-files --others --exclude-standard | xargs ls -l" (current-buffer))
  (dired-mode dir)
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker)))))
(put 'narrow-to-region 'disabled nil)


;;;;;;;;;;;;;;;
;; * Folding ;;
;; ;;;;;;;;;;;;;;;

;; (defun aj-toggle-fold () 
;;   "Toggle fold all lines larger than indentation on current line" 
;;   (interactive) 
;;   (let ((col 1)) 
;;     (save-excursion 
;;       (back-to-indentation) 
;;       (setq col (+ 1 (current-column))) 
;;       (set-selective-display 
;;        (if selective-display nil (or col 1))))))
;; (global-set-key (kbd "C-x M-f") 'aj-toggle-fold)

;; No confirmation on buffer kill
(global-set-key (kbd "C-x k") 'kill-this-buffer)


;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)


;; ZTREE: compare folders
(push (substitute-in-file-name "path-to-ztree-directory") load-path)
(require 'ztree)


;; ;;; Reclaim digits for shortcut keys
;; ;;;  (http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/)
;; ;; unset C- and M- digit keys
;; (dotimes (n 10)
;;   (global-unset-key (kbd (format "C-%d" n)))
;;   (global-unset-key (kbd (format "M-%d" n)))
;;   )
;; ;; ;; set up my own map
;; ;; (define-prefix-command 'bjm-map)
;; ;; (global-set-key (kbd "C-1") 'bjm-map)
;; ;; (define-key bjm-map (kbd "m") 'mu4e)
;; (global-set-key (kbd "C-1") 'avy-goto-char-timer)


;;; dired quick sort - press S in dired for sorting options
(require 'dired-quick-sort)
(dired-quick-sort-setup)

;;; Easy navigation
;; (require 'outshine)
;; (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(add-hook 'prog-mode-hook 'outshine-mode)
;; (require 'outshine-mode)
(require 'navi-mode)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'ess-mode-hook 'outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(add-hook 'c++-mode-hook 'outline-minor-mode)
(add-hook 'message-mode-hook 'outline-minor-mode)
(defvar outline-minor-mode-prefix "\M-#")

;; (use-package dumb-jump
;;   :ensure t
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;; 	 ("M-g j" . dumb-jump-go)
;; 	 ("M-g b" . dumb-jump-back)
;; 	 ("M-g q" . dumb-jump-quick-look)
;; 	 ("M-g x" . dumb-jump-go-prefer-external)
;; 	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config (setq dumb-jump-selector 'helm)
;;   )
(define-key dumb-jump-mode-map (kbd "C-M-q") nil)
(define-key dumb-jump-mode-map (kbd "C-M-j") 'dumb-jump-quick-look)
(setq dumb-jump-selector 'helm)
(setq dumb-jump-prefer-searcher 'ag)

(global-set-key (kbd "C-s") 'helm-swoop)
;; (global-set-key (kbd "C-s") 'helm-occur)


;; (when (window-system)
;;   (set-default-font "Ubuntu Mono"))

;; (add-hook 'makefile-mode-hook 'turn-off-flyspell t)
;; (add-hook 'emacs-lisp-mode-hook 'turn-off-flyspell t)
;; (add-hook 'ess-mode-hook 'turn-off-flyspell t)


(global-set-key (kbd "C-x M-a") 'helm-do-ag)


;; for graphviz in org
(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)

;; (load "~/.emacs.d/org-mind-map.el")


;; (require 'helm-projectile)
;; (helm-projectile-on)



;; Change colors for printing with ps-print-*
(ps-extend-face '(default "#000000" nil) 'MERGE)
(ps-extend-face '(font-lock-variable-name-face "#984EA3" nil) 'MERGE)
(ps-extend-face '(font-lock-function-name-face "#084081" nil) 'MERGE)
(ps-extend-face '(rainbow-delimiters-depth-3-face "#FF7F00" nil) 'MERGE)



;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window))
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "#313131"))))))
;;   (buffer-face-set 'default))
;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)


;; (put 'scroll-left 'disabled nil)


;; (latex-preview-pane-enable)
(setq-default TeX-engine 'xetex)

(global-set-key (kbd "s-P") 'powerthesaurus-lookup-word-dwim)


;; ;; ADMB mode
;; (add-to-list 'load-path "~/.emacs.d/admb")
;; (autoload 'admb-mode "admb" "Edit ADMB code." t)
;; (add-to-list 'auto-mode-alist '("\\.tpl$" . admb-mode))

(setq ess-smart-S-assign-key nil)
(define-key ess-mode-map (kbd ";") 'ess-insert-assign)
(define-key inferior-ess-mode-map (kbd ";") 'ess-insert-assign)


;; (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+R-mode))
;; (add-to-list 'auto-mode-alist '("\\.rmd" . poly-markdown+r-mode))
;; (require 'poly-R)
;; (require 'poly-markdown)
;; (require 'R-mode)

(server-start)

;; (defun rmd-mode ()
;;   "ESS Markdown mode for rmd files"
;;   (interactive)
;;   ;; (setq load-path 
;;   ;;   (append (list "path/to/polymode/" "path/to/polymode/modes/")
;;   ;;       load-path))
;;   (require 'poly-R)
;;   (require 'poly-markdown)     
;;   (poly-markdown+r-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
;; (add-to-list 'auto-mode-alist '("\\.Rmd" . rmd-mode))


(put 'erase-buffer 'disabled nil)


;; * Psession - alternative to desktop-mode to save buffers
(psession-mode 1)
(psession-autosave-mode 1)


(defun basename (fname)
  (if (or (file-directory-p fname)
          (string-match "/$" fname))
      (let ((dirname (directory-file-name fname))) 
        (file-name-nondirectory dirname))
      (file-name-nondirectory fname)))

(defun er-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (basename default-directory)
                    (file-name-nondirectory buffer-file-name))))
    (when filename
      (kill-new (file-name-nondirectory filename))
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c p") 'er-copy-file-name-to-clipboard)



;;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ronn?" . markdown-mode) auto-mode-alist))

;; Polymode and co
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(defun ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
	      (format "library(rmarkdown); rmarkdown::render(\"%s\")"
		      buffer-file-name)))
	(message "Running rmarkdown on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))


(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))


;; ;; * from https://with-emacs.com/

;; (autoload 'View-scroll-half-page-forward "view")
;; (autoload 'View-scroll-half-page-backward "view")
;; (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
;; (global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;; (global-set-key (kbd "C-M-v")
;;   'my-View-scroll-half-page-forward-other-window)
;; (global-set-key (kbd "C-M-S-v")
;;   'my-View-scroll-half-page-backward-other-window)

;; (defun my-View-scroll-half-page-forward-other-window ()
;;   (interactive)
;;   (with-selected-window (next-window)
;;     (call-interactively 'View-scroll-half-page-forward)))

;; (defun my-View-scroll-half-page-backward-other-window ()
;;   (interactive)
;;   (with-selected-window (next-window)
;;     (call-interactively 'View-scroll-half-page-backward)))

;; (setq scroll-preserve-screen-position 'always)

;; (advice-add #'View-scroll-half-page-forward :around
;;             #'my-indicate-scroll-forward)

;; (advice-add #'View-scroll-half-page-backward :around
;;             #'my-indicate-scroll-backward)

;; (defun my-indicate-scroll-get-line (pos)
;;   (save-excursion
;;     (goto-char pos)
;;     (string-to-number (format-mode-line "%l"))))

;; (defun my-indicate-scroll (linep f args)
;;   (let ((linen (my-indicate-scroll-get-line linep))
;;         (pulse-delay 0.1))
;;     (save-excursion
;;       (goto-line linen)
;;       (pulse-momentary-highlight-one-line (point) 'highlight))
;;     (sit-for 0.1)
;;     (apply f args)))

;; (defun my-indicate-scroll-forward (f &rest args)
;;   (my-indicate-scroll (1- (window-end)) f args))

;; (defun my-indicate-scroll-backward (f &rest args)
;;   (my-indicate-scroll (window-start) f args))


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

(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)

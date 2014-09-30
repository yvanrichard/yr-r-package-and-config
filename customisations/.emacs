;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)    ; now you should get a backtrace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path
    "~/.emacs.d")
;; (add-to-list 'load-path
;;     "~/.emacs.d/icicles")
;; (add-to-list 'load-path
;;     "~/.emacs.d/elisp-buffer-timer")

(require 'package)
(package-initialize)
(setq package-archives
'(("ELPA" . "http://tromey.com/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))
   ;; ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Window position
(set-face-attribute 'default (selected-frame) :height 110)
(add-to-list 'default-frame-alist '(width . 210))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(left . 100))
(add-to-list 'default-frame-alist '(top . 50))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Global
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [C-tab] 'completion-at-point)
(global-set-key (kbd "C-\\") 'dabbrev-expand)

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
(global-set-key "\C-a" 'back-to-indentation)

;; For completion in ESS (keybinding conflit with TAB)
;;(global-set-key (kbd "C-'") 'completion-at-point)


;; Search in multiple buffers (first argument is regexp on buffer names)
(global-set-key "\C-c\M-m" 'multi-occur-in-matching-buffers)

(setq doc-view-continuous t)

;; use autofill on text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; (require 'save-visited-files)
;; (turn-on-save-visited-files-mode)

;; (add-to-list 'load-path "~/.emacs.d/predictive/")
;; (add-to-list 'load-path "~/.emacs.d/predictive/latex/")
;; (require 'predictive)
;; ;http://www.emacswiki.org/emacs/PredictiveMode
;; (autoload 'predictive-mode "predictive" "predictive" t)
;; (set-default 'predictive-auto-add-to-dict t)
;; (setq predictive-main-dict 'rpg-dictionary
;;       predictive-auto-learn t
;;       predictive-add-to-dict-ask nil
;;       predictive-use-auto-learn-cache nil
;;       predictive-which-dict t)
(require 'auto-complete)

(setq max-lisp-eval-depth 10000)

(require 'pabbrev)

(global-set-key [f9] 'compile)

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
;;    Colours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'wombat)
;; (load-theme 'tango-dark)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline nil :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "PeachPuff"))))
 '(font-lock-string-face ((t (:foreground "DarkSeaGreen2"))))
 '(font-lock-type-face ((t (:foreground "CadetBlue1"))))
 '(org-level-4 ((t (:foreground "khaki1" :inherit (outline-4)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "darksalmon"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "LightGoldenrod4")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-biblatex-use-Biber t)
 '(ess-R-font-lock-keywords (quote ((ess-R-fl-keyword:modifiers . t) (ess-R-fl-keyword:fun-defs . t) (ess-R-fl-keyword:keywords . t) (ess-R-fl-keyword:assign-ops . t) (ess-R-fl-keyword:constants . t) (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t) (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t) (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t))))
 '(hl-sexp-background-color "#251D25")
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((require-final-newline))))
 '(wakatime-api-key "7b9228e4-0256-4dfe-a43e-1bfb09f7288c")
 '(wakatime-cli-path "/home/yvan/wakatime/wakatime-cli.py")
 '(yank-pop-change-selection t))

(global-wakatime-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Latex, Sweave, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use reftex
  ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
  ; with Emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)
  ; use natural science bibliography style
(setq reftex-cite-format 'natbib)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(setq latex-run-command "xelatex")

;; (setq reftex-file-extensions
;;       '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
;; (setq TeX-file-extensions
;;       '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))

;; (setq ispell-program-name "ispell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

;; (global-set-key (kbd "M-Q") 'region-fill-as-paragraph)                                  

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'LaTeX-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'dabbrev-expand)))
(add-hook 'latex-mode-hook
	  '(lambda()
	     (local-set-key [(tab)] 'dabbrev-expand)))

(add-to-list 'ispell-skip-region-alist '("^<<.*>>=" . "^@"))
;; (add-to-list 'ispell-skip-region-alist '("Sexpr{" . "}"))
(defun flyspell-eligible ()
  (let ((p (point)))
    (save-excursion
      (cond ((re-search-backward (ispell-begin-skip-region-regexp) nil t)
             (ispell-skip-region (match-string-no-properties 0))
             (< (point) p))
            (t)))))
(put 'latex-mode 'flyspell-mode-predicate 'flyspell-eligible)
(put 'LaTeX-mode 'flyspell-mode-predicate 'flyspell-eligible)
(put 'Rnw-mode 'flyspell-mode-predicate 'flyspell-eligible)
(put 'ess-mode 'flyspell-mode-predicate 'flyspell-eligible)

(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))

(require 'latex-frame-mode)



(defun flyspell-ignore-verbatim ()
  "Function used for `flyspell-generic-check-word-predicate' to ignore {{{ }}} blocks."
  (save-excursion
    (widen)
    (let ((p (point))
          (count 0))
      (not (or (and (re-search-backward "^<<" nil t)
                    (> p (point))
                    ;; If there is no closing }}} then assume we're still in it
                    (or (not (re-search-forward "^@" nil t))
                        (< p (point))))
               (eq 1 (progn (while (re-search-backward "`" (line-beginning-position) t)
                              (setq count (1+ count)))
                            (- count (* 2 (/ count 2))))))))))
(put 'latex-mode 'flyspell-mode-predicate 'flyspell-ignore-verbatim)

(setq reftex-default-bibliography '("/home/yvan/dragonfly/bibliography/mfish.bib"))

;; (setq safe-local-variable-values ((TeX-master . "report.tex")
;;  (TeX-master . "report")
;;  (TeX-master . t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ido-mode (autocompletion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ido-mode t)
(setq ido-enable-flex-matching t)

;; Flex ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    utils for finding non-ascii characters
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
;;    i-buffer
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
	 ("Northern royals" (filename . "northern-royal"))
	 ("Oreo" (filename . "oreo"))
	 ("Seabirds 2013" (filename . "seabirds-2013"))
	 ("Bycatch www" (filename . "bycatch.dragonfly"))
	 ("Seabird counts" (filename . "abundance/"))
	 ("Maui's dolphins" (filename . "maui"))
	 ("Seabird counts website" (filename . "seabird-counts-website"))
	 ("XBP distribution" (filename . "black-petrel-distribution"))
	 ("MBIE" (or (filename . "eREAR")
		     (filename . "mbie")))
	 ("SRA 2012" (filename . "sra-2012"))
	 ("SRA 2014" (filename . "sra-2014"))
	 ("Ludicio" (filename . "ludicio/"))
	 ("emacs-config" (or (filename . ".emacs.d")
			     (filename . "emacs-config")
			     (filename . ".emacs"))))))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Code folding & indentation & formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; Highlight region between parentheses
;; (require 'paren)
;; (set-face-background 'show-paren-match-face "#696969")
;; (set-face-foreground 'show-paren-match-face "#def")
;; (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
;; (setq show-paren-delay 0)
;; (show-paren-mode 1)

(require 'highlight-indentation)
(add-hook 'ess-mode-hook 'highlight-indentation-mode) 
(add-hook 'lisp-mode-hook 'highlight-indentation-mode)

(defun aj-toggle-fold () 
  "Toggle fold all lines larger than indentation on current line" 
  (interactive) 
  (let ((col 1)) 
    (save-excursion 
      (back-to-indentation) 
      (setq col (+ 1 (current-column))) 
      (set-selective-display 
       (if selective-display nil (or col 1))))))
(global-set-key (kbd "M-C-i") 'aj-toggle-fold)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'ess-jags-d)
(autoload 'ess-jags-mode "ess-jags-mode"
   "Major mode for editing JAGS files" t)
(add-to-list 'auto-mode-alist '("\\.bug\\'" . ess-jags-mode))

(require 'ess-eldoc) ;to show function arguments while you are typing them

(require 'ess-site)
;; (load "~/.emacs.d/elpa/ess-20130912.915/lisp/ess-site")

(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
	(delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1 nil t))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'my-ess-eval)
	     (local-set-key [(backtab)] 'ess-indent-or-complete)))
(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     (local-set-key [C-up] 'comint-previous-input)
	     (local-set-key [C-down] 'comint-next-input)))

(ess-toggle-underscore nil)
(setq ess-S-assign-key (kbd "C-="))
(ess-toggle-S-assign-key t)
;; (ess-auto-newline t)

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

(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.rnw\\'" . Rnw-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'anything-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-src-fontify-natively t)
(setq org-support-shift-select t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    uniquify (change file<2> to file/fold)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify) 
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically save and restore sessions
(setq desktop-dirname             "/home/yvan/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "desktoplock"
      desktop-path                (list desktop-dirname)
      )
(desktop-save-mode 1)
(desktop-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Copy buffer file path to clipboad
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
;;    Wrap region  (https://github.com/rejeep/wrap-region.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(wrap-region-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Ace jump mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(helm-mode -1)
(global-set-key (kbd "C-c h") 'helm-mini)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    expand-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'expand-region) 
(global-set-key (kbd "C-'") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    yas snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq yas-snippet-dirs
      '("~/Dropbox/customisations/yas-snippets"))
(add-to-list 'load-path "~/Dropbox/customisations/yas-snippets")
(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'powerline)
(powerline-default-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Highlight sexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-highlight-sexp-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Scroll bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-yascroll-bar-mode 1)
;; (scroll-bar-mode 0)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Abbreviations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-abbrev-table 'global-abbrev-table '(

    ;; normal english words
    ("0pop" "population")
    ("0vul" "vulnerability")
    ("0sp"  "species")
    ("0xwm" "New Zealand white-capped albatross")
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
    ("apf"  "APF")
    ("0apf" "Annual Potential Fatalities")
    ("0nz"  "New Zealand")
    ("0ssp" "subspecies")
    ("0bll" "bottom longline")
    ("0sll" "surface longline")
    ("0sra2t" "\citet{richard_risk_2013}")
    ("0sra2p" "\citep{richard_risk_2013}")
    
    ))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)


;; ;; Increase quality of viewed pdfs
;; ;; (defcustom doc-view-ghostscript-options
;; ;;           '("-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4"
;; ;;             "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET"
;; ;;             "-r300")
;; ;;           "A list of options to give to ghostview."
;; ;;           :type '(sexp)
;; ;;           :group 'doc-view)

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

;; ;; (put 'erase-buffer 'disabled nil)

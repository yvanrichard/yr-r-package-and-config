;; (require 'color-theme)
;; (load-file "~/.emacs.d/my-color-theme.el")
;; (my-color-theme)
(add-to-list 'load-path
    "~/.emacs.d")
(add-to-list 'load-path
    "~/.emacs.d/icicles")
(add-to-list 'load-path
    "~/.emacs.d/elisp-buffer-timer")


(require 'package)
(package-initialize)
(setq package-archives
'(("ELPA" . "http://tromey.com/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")))

;; key to switch between windows
(global-set-key (kbd "s-`") 'other-window)

;; Window position
(set-face-attribute 'default (selected-frame) :height 110)
(add-to-list 'default-frame-alist '(width . 210))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(left . 100))
(add-to-list 'default-frame-alist '(top . 50))

;; highlight brackets
(require 'paren)
(show-paren-mode 1)

;; colour highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; yassnipped
(require 'yasnippet)
(yas--initialize)
(yas/load-directory "~/.emacs.d/elpa/yasnippet-0.8.0/snippets")
(require 'r-autoyas)
(require 'icicles)
(require 'ess-eldoc) ;to show function arguments while you are typing them


;;; ESS
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
	     (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     (local-set-key [C-up] 'comint-previous-input)
	     (local-set-key [C-down] 'comint-next-input)))
;; (require 'ess-site)
(load "~/.emacs.d/ess-12.09-2/lisp/ess-site")

(ess-toggle-underscore nil)
(setq ess-S-assign-key [?\C-=])
(ess-toggle-S-assign-key t)
;; (ess-auto-newline t)




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


;; rebind to use buffer-menu instead of list-buffers 
;; to keep everything in same buffer
(global-set-key "\C-x\C-b" 'ibuffer)
(setq ibuffer-default-sorting-mode 'filename/process)


(defun kill-other-window ()
  "Kill the buffer in the other pane."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  )
(global-set-key "\C-c\C-w" 'kill-other-window)

(global-set-key [C-tab] 'comint-dynamic-complete)


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline nil :weight bold))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height 1.05))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :weight extra-bold :height 1.05))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face :weight normal))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "yellow" :weight bold :foundry "sans" :family "mono"))))
 '(font-lock-type-face ((t (:foreground "navajowhite")))))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))


;; (add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
;; (add-to-list 'auto-mode-alist '("\\.rnw\\'" . Rnw-mode))
;; (add-to-list 'auto-mode-alist '("\\.Snw\\'" . Rnw-mode))

;; use reftex
  ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
  ; with Emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)
  ; use natural science bibliography style
(setq reftex-cite-format 'natbib)
(setq-default TeX-master nil)

;; (setq reftex-file-extensions
;;       '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
;; (setq TeX-file-extensions
;;       '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))

(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

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

(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))

;; (defun flyspell-ignore-verbatim ()
;;   "Function used for `flyspell-generic-check-word-predicate' to ignore {{{ }}} blocks."
;;   (save-excursion
;;     (widen)
;;     (let ((p (point))
;;           (count 0))
;;       (not (or (and (re-search-backward "^<<" nil t)
;;                     (> p (point))
;;                     ;; If there is no closing }}} then assume we're still in it
;;                     (or (not (re-search-forward "^@" nil t))
;;                         (< p (point))))
;;                (eq 1 (progn (while (re-search-backward "`" (line-beginning-position) t)
;;                               (setq count (1+ count)))
;;                             (- count (* 2 (/ count 2))))))))))
;; (put 'latex-mode 'flyspell-mode-predicate 'flyspell-ignore-verbatim)


(load-theme 'wombat)
;; (load-theme 'tango-dark)

(global-set-key "\C-z" nil)  ;; prevent emacs from being minimized with C-Z

(global-set-key [C-prior] 'previous-buffer)                                  
(global-set-key [C-next] 'next-buffer)                                 
(global-set-key "\C-a" 'back-to-indentation)

;; use autofill on text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; (require 'save-visited-files)
;; (turn-on-save-visited-files-mode)

;; (require 'desktop)
(desktop-save-mode 1)
(desktop-read)
;; (require 'highlight-chars)

;; set sorting column
(setq Buffer-menu-sort-column 4)



(defun comint-interrupt-subjob-other ()
  "Interrupt process in the other pane."
  (interactive)
  (other-window 1)
  (comint-interrupt-subjob)
  (other-window -1)
  )
(global-set-key "\C-c\C-q" 'comint-interrupt-subjob-other)


(require 'buffer-timer)

; Example list of titles and regexps to group by.  This
(setq buffer-timer-regexp-master-list
  '(
    ("idle" . 
     (("generic" .			  "^\\*idle\\*")
      ("generic2" .			  "^\\*idle-2\\*")
      ("minibuf" .                        "^ \\*Minibuf-.*")))
    ("customizations" .                   "\\.emacs")
    ("work" .
     (("my R pkg" .                       "yr-r-package")
      ("seabird counts" .
       (("Main project" .                 "abundance")
	("Web site" .                     "seabird-counts-website")))
      ("SRA 2012" .
       (("analysis" .                     "sra-2012/analysis")
	("report" .                       "sra-2012/report")))
      ("Encounter kaikoura" .
       (("data" .                          "encounter-kaikoura/data")
	("analysis" .                      "encounter-kaikoura/analysis")
	("plots" .                         "encounter-kaikoura/plots")
	("report" .                        "encounter-kaikoura/report")))
      ("XNR Taiaroa" .
       (("data" .                          "northern-royal-albatross-taiaroa/data")
	("analysis" .                      "northern-royal-albatross-taiaroa/analysis")
	("plots" .                         "northern-royal-albatross-taiaroa/plots")
	("report" .                        "northern-royal-albatross-taiaroa/report")))
      ("R terminal" .                     "^\\*R\\*$")
      ("Dragonfly others" .               "/dragonfly/")))
    )
)

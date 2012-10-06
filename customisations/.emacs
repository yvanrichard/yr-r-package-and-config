(add-to-list 'load-path
    "~/.emacs.d/plugins")

;;; yasnippet
;; (add-to-list 'load-path
;;     "~/.emacs.d/plugins/yasnippet-fdf7582")
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/plugins/yasnippet-fdf7582/snippets")

(require 'r-autoyas)
;(require 'ac-R)

;; key to switch between windows
(global-set-key (kbd "s-`") 'other-window)

;;; icicles
(add-to-list 'load-path
    "~/.emacs.d/plugins/emacsmirror-icicles-43fc99c")
(require 'icicles)

(require 'color-theme)
(load-file "~/.emacs.d/my-color-theme.el")
(my-color-theme)
;(color-theme-tango)
;;(add-hook 'after-init-hook 'color-theme-tango)
;;(color-theme-jsc-dark)

(autoload 'tex-math-preview "tex-math-preview" nil t)
;; (add-hook 'texinfo-mode-hook
;;      (lambda ()
;;        (define-key texinfo-mode-map [f8] 'tex-math-preview)))

;; Window position
(set-face-attribute 'default (selected-frame) :height 110)
(add-to-list 'default-frame-alist '(width . 210))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(left . 100))
(add-to-list 'default-frame-alist '(top . 50))

(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

;(setq split-height-threshold nil)
;(setq split-width-threshold 0)

;; For outline mode
(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c\C-o") ; Or something else

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(setq-default TeX-master nil)
(visual-line-mode t)

;; highlight brackets
(require 'paren)
(show-paren-mode 1)

;; colour highlighting
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t)

;; use autofill on text modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use auctex
;; (require 'tex-site)

;; use reftex
  ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
  ; with Emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)
  ; use natural science bibliography style
(setq reftex-cite-format 'natbib)


;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(inhibit-startup-screen t))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#222222" :foreground "#FFFFFF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(global-hl-line-mode -1)
(line-number-mode 1)
;(set-background-color "black")
(set-scroll-bar-mode 'right)

;; (load-file "~/.emacs.d/my-color-theme.el")
;; (my-color-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(inhibit-startup-screen t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height 1.05))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :weight extra-bold :height 1.05))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face :weight normal))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "yellow" :weight bold :foundry "sans" :family "mono"))))
 '(font-lock-type-face ((t (:foreground "navajowhite" :weight normal :height 1.0 :family "DejaVu Sans Mono")))))
 
;;; ESS
(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session
(require 'ess-eldoc) ;to show function arguments while you are typing them

;;; key stroke for opening R if not running, evaluate region, or evaluate current line if there is no region
;;; http://www.emacswiki.org/emacs/ESSShiftEnter
;(setq ess-ask-for-ess-directory nil)
;  (setq ess-local-process-name "R")
;  (setq ansi-color-for-comint-mode 'filter)
;  (setq comint-prompt-read-only t)
;  (setq comint-scroll-to-bottom-on-input t)
;  (setq comint-scroll-to-bottom-on-output t)
;  (setq comint-move-point-for-output t)
;
;  (defun my-ess-start-R ()
;    (interactive)
;    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
;      (progn
;	(delete-other-windows)
;	(setq w1 (selected-window))
;	(setq w1name (buffer-name))
;	(setq w2 (split-window w1))
;	(R)
;	(set-window-buffer w2 "*R*")
;	(set-window-buffer w1 w1name))))
;
;  (defun my-ess-eval ()
;    (interactive)
;    (my-ess-start-R)
;    (if (and transient-mark-mode mark-active)
;	(call-interactively 'ess-eval-region)
;      (call-interactively 'ess-eval-line-and-step)))
;
;  (add-hook 'ess-mode-hook
;	    '(lambda()
;	       (local-set-key [(shift return)] 'my-ess-eval)))
;
;  (add-hook 'inferior-ess-mode-hook
;	    '(lambda()
;	       (local-set-key [C-up] 'comint-previous-input)
;	       (local-set-key [C-down] 'comint-next-input)))
;  (require 'ess-site)

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
(require 'ess-site)


;; ess-R-object-tooltip.el
;; http://www.sigmafield.org/2009/10/01/r-object-tooltips-in-ess/
;;
;; I have defined a function, ess-R-object-tooltip, that when 
;; invoked, will return a tooltip with some information about
;; the object at point.  The information returned is 
;; determined by which R function is called.  This is controlled
;; by an alist, called ess-R-object-tooltip-alist.  The default is
;; given below.  The keys are the classes of R object that will
;; use the associated function.  For example, when the function
;; is called while point is on a factor object, a table of that
;; factor will be shown in the tooltip.  The objects must of course
;; exist in the associated inferior R process for this to work.
;; The special key "other" in the alist defines which function
;; to call when the class is not mached in the alist.  By default,
;; the str function is called, which is actually a fairly useful
;; default for data.frame and function objects. 
;; 
;; The last line of this file shows my default keybinding. 
;; I simply save this file in a directory in my load-path
;; and then place (require 'ess-R-object-tooltip) in my .emacs 

;; the alist
(setq ess-R-object-tooltip-alist
      '((numeric    . "summary")
        (factor     . "table")
        (integer    . "summary")
        (lm         . "summary")
        (other      . "str")))


(defun ess-R-object-tooltip ()
  "Get info for object at point, and display it in a tooltip."
  (interactive)
  (let ((objname (current-word))
        (curbuf (current-buffer))
        (tmpbuf (get-buffer-create "**ess-R-object-tooltip**")))
    (if objname
        (progn
          (ess-command (concat "class(" objname ")\n")  tmpbuf )   
          (set-buffer tmpbuf)
          (let ((bs (buffer-string)))
            (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                (let* ((objcls (buffer-substring 
                                (+ 2 (string-match "\".*\"" bs)) 
                                (- (point-max) 2)))
                       (myfun (cdr(assoc-string objcls 
                                                ess-R-object-tooltip-alist))))
                  (progn
                    (if (eq myfun nil)
                        (setq myfun 
                              (cdr(assoc-string "other" 
                                                ess-R-object-tooltip-alist))))
                    (ess-command (concat myfun "(" objname ")\n") tmpbuf)
                    (let ((bs (buffer-string)))
                      (progn
                        (set-buffer curbuf)
                        (tooltip-show-at-point bs 0 30)))))))))
    (kill-buffer tmpbuf)))

;; my default key map
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-tooltip)

(provide 'ess-R-object-tooltip)

(ess-toggle-underscore nil)
(setq ess-S-assign-key [?\C-=])
(ess-toggle-S-assign-key t)

(add-to-list 'ispell-skip-region-alist '("^<<.*>>=" . "^@"))
(defun flyspell-eligible ()
  (let ((p (point)))
    (save-excursion
      (cond ((re-search-backward (ispell-begin-skip-region-regexp) nil t)
             (ispell-skip-region (match-string-no-properties 0))
             (< (point) p))
            (t)))))
(put 'latex-mode 'flyspell-mode-predicate 'flyspell-eligible)

(global-set-key "\C-z" nil)  ;; prevent emacs from being minimized with C-Z



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

(server-start)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)


(add-to-list 'load-path "okular-search.el")
(require 'okular-search)
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\C-c\C-v" 'okular-jump-to-line)))
(add-hook 'tex-mode-hook (lambda () (local-set-key "\C-c\C-v" 'okular-jump-to-line)))

;; rebind to use buffer-menu instead of list-buffers to keep everything in same buffer
(global-set-key "\C-x\C-b" 'buffer-menu)


;; (add-hook 'inferior-ess-mode-hook 
;; 	  '(lambda nil 
;; 	     (define-key inferior-ess-mode-map [\C-up] 
;; 	       'comint-previous-matching-input-from-input) 
;; 	     (define-key inferior-ess-mode-map [\C-down] 
;; 	       'comint-next-matching-input-from-input) 
;; 	     (define-key inferior-ess-mode-map [\C-x \t] 
;; 	       'comint-dynamic-complete-filename) 
;; 	     ) 
;; 	  )
(defun kill-other-window ()
  "Kill the buffer in the other pane."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window -1)
  )
(global-set-key "\C-c\C-w" 'kill-other-window)

(global-set-key [C-tab] 'comint-dynamic-complete)

Demos: see https://github.com/cute-jumper/ace-jump-helm-line

                          ____________________

                           ACE-JUMP-HELM-LINE

                              Junpeng Qiu
                          ____________________


Table of Contents
_________________

1 Setup
2 Usage
3 Acknowledgment


Ace-jump to a candidate in helm window.

This package makes use of the `avy.el'.


1 Setup
=======

  ,----
  | (add-to-list 'load-path "/path/to/ace-jump-helm-line.el")
  | (require 'ace-jump-helm-line)
  `----

  You can use the following code to bind `ace-jump-helm-line' (or
  `ace-jump-helm-line-execute-action'. See later) to a key(say, "C-'"):
  ,----
  | (eval-after-load "helm"
  | '(define-key helm-map (kbd "C-'") 'ace-jump-helm-line))
  `----


2 Usage
=======

  When in a helm session, for example, after you call `helm-M-x', you
  can use your key binding(for example, "C-'") to invoke
  `ace-jump-helm-line'. See demos.

  `ace-jump-helm-line' would just jump to that candidate in helm window.
  If you want to jump to and automatically select the candidate, which
  will then quit the helm session, you can bind the key to
  `ace-jump-helm-line-execute-action'.

  There are two kinds of styles: avy-jump style and ace-jump-mode style.
  By default, this package uses `avy-jump' style(anyway, it uses
  `avy-jump.el'!). You can certainly change to `ace-jump-mode-style' by:
  ,----
  | (setq ace-jump-helm-line-use-avy-style nil)
  `----


3 Acknowledgment
================

  - Thank [Oleh Krehel] for the awesome [avy] package.
  - Thank @hick for the original idea.


  [Oleh Krehel] https://github.com/abo-abo/

  [avy] https://github.com/abo-abo/avy

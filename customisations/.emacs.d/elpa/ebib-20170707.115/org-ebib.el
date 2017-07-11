;;; org-ebib.el --- Support for links to Ebib entries in Org  -*- lexical-binding: t -*-

;; Copyright (c) 2014-2016 Grégoire Jadi, Joost Kremers
;; Al rights reserved

;; Author: Grégoire Jadi <daimrod@gmail.com>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides two functions to integrate Ebib with org-mode.
;; `org-ebib-open' can be called with a BibTeX key as argument.  It opens Ebib
;; and shows the entry corresponding to the key.  `org-ebib-store-link' creates
;; a link that when followed opens Ebib.

;;; Code:

(require 'org)

;; This is to silence the byte-compiler and flycheck.
(defvar ebib--cur-db)
(defvar ebib-citation-description-function)
(defvar reftex-cite-punctuation)
(declare-function ebib "ebib" (&optional file key))
(declare-function ebib--get-key-at-point "ebib" ())
(declare-function org-link-set-parameters "org" (type &rest parameters))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property element))
(declare-function ebib-db-get-entry "ebib-db" (key db &optional noerror))
(declare-function reftex-format-citation "reftex-cite" (entry format))

(org-link-set-parameters "ebib" :follow #'org-ebib-open :store #'org-ebib-store-link)

;;;###autoload
(defun org-ebib-open (key)
  "Open Ebib and jump to KEY."
  (ebib nil key))

(defun org-ebib-store-link ()
  "Store a link to an Ebib entry."
  (when (memq major-mode '(ebib-index-mode ebib-entry-mode))
    ;; This is an Ebib entry
    (let* ((key (ebib--get-key-at-point))
           (link (concat "ebib:" key))
           (description (ignore-errors (funcall ebib-citation-description-function key ebib--cur-db))))
      (org-store-link-props :type "ebib"
                            :link link
                            :description description))))

(with-eval-after-load "reftex-cite"
  (org-link-set-parameters "ebib" :help-echo #'org-ebib-show-citation-info))

(defun org-ebib-show-citation-info (window _object position)
  "Help echo function for org-ebib links.
WINDOW is the window displaying the link, POSITION the link's
position in the relevant buffer."
  (with-selected-window window
    (save-excursion
      (goto-char position)
      (goto-char (org-element-property :begin (org-element-context)))
      (cond
       ((looking-at org-bracket-link-regexp)
        (mapconcat 'org-ebib-make-help-echo-string
                   (split-string
                    (cadr (split-string (match-string-no-properties 1)
                                        ":" t "[[:punct:]]*"))
                    "," t)
                   "\n"))
       ((looking-at org-plain-link-re)
        (mapconcat 'org-ebib-make-help-echo-string
                   (split-string
                    (cadr (split-string (match-string-no-properties 0)
                                        ":" t "[[:punct:]]*"))
                    "," t)
                   "\n"))
       (t "Not a link?")))))

(defcustom org-ebib-help-echo-format "%2a (%y), %t, %b, %j %<"
  "Citation format used to display citation info in the message area.
Must NOT contain %l.  See the variable `reftex-cite-format' for
possible percent escapes."
  :group 'ebib
  :type 'string)

(defun org-ebib-make-help-echo-string (key &optional format)
  "Return the citation string of KEY according to FORMAT.
It is adapted from `reftex-make-cite-echo-string'."
  (let ((entry (ebib-db-get-entry key ebib--cur-db 'noerror))
        (reftex-cite-punctuation '(" " " & " " et al.")))
    (or format (setq format org-ebib-help-echo-format))
    (if entry
        (replace-regexp-in-string
         "[\"{}]" ""
         (reftex-format-citation entry format))
      (format "Key %s is not found in current database." key))))

(provide 'org-ebib)

;;; org-ebib.el ends here

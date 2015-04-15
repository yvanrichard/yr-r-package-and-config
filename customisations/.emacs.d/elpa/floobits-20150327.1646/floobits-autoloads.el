;;; floobits-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (floobits-add-to-workspace floobits-clear-highlights
;;;;;;  floobits-open-workspace-in-browser floobits-remove-from-workspace
;;;;;;  floobits-workspace-settings floobits-join-workspace floobits-share-dir-private
;;;;;;  floobits-share-dir-public floobits-complete-signup floobits-leave-workspace
;;;;;;  floobits-follow-user floobits-follow-mode-toggle floobits-summon
;;;;;;  floobits-debug) "floobits" "floobits.el" (21788 21191 354465
;;;;;;  725000))
;;; Generated autoloads from floobits.el

(autoload 'floobits-debug "floobits" "\
Toggles debug logging.

\(fn)" t nil)

(autoload 'floobits-summon "floobits" "\
Summons all users to your cursor position.

\(fn)" t nil)

(autoload 'floobits-follow-mode-toggle "floobits" "\
Toggles following of recent changes in a workspace

\(fn)" t nil)

(autoload 'floobits-follow-user "floobits" "\
Follow a users changes. This also toggles follow mode.

\(fn)" t nil)

(autoload 'floobits-leave-workspace "floobits" "\
leaves the current workspace

\(fn)" t nil)

(autoload 'floobits-complete-signup "floobits" "\
If you created an Floobits account via emacs, you must call this command before you can login to
  the website.

\(fn)" t nil)

(autoload 'floobits-share-dir-public "floobits" "\
Create a workspace and populate it with the contents of the directory, dir-to-share, or make it.
If the directory corresponds to an existing floobits workspace, you will instead join the workspace.

\(fn DIR-TO-SHARE)" t nil)

(autoload 'floobits-share-dir-private "floobits" "\
Create a workspace and populate it with the contents of the directory, dir-to-share, or make it.
If the directory corresponds to an existing floobits workspace, you will instead join the workspace.

\(fn DIR-TO-SHARE)" t nil)

(autoload 'floobits-join-workspace "floobits" "\
Join an existing floobits workspace.
See floobits-share-dir to create one or visit floobits.com.

\(fn FLOOURL)" t nil)

(autoload 'floobits-workspace-settings "floobits" "\


\(fn)" t nil)

(autoload 'floobits-remove-from-workspace "floobits" "\
Removes a file from the remote workspace without deleting it locally

\(fn PATH)" t nil)

(autoload 'floobits-open-workspace-in-browser "floobits" "\


\(fn)" t nil)

(autoload 'floobits-clear-highlights "floobits" "\
Clears all highlights

\(fn)" t nil)

(autoload 'floobits-add-to-workspace "floobits" "\
Adds a file or directory to the workspace

\(fn PATH)" t nil)

;;;***

;;;### (autoloads nil nil ("floobits-pkg.el") (21788 21191 467477
;;;;;;  986000))

;;;***

(provide 'floobits-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; floobits-autoloads.el ends here

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load my local copy of org-mode, not the version included with Emacs
(push (expand-file-name "vendor/org-mode/lisp" user-emacs-directory) load-path)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "stuart.org" user-emacs-directory))
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

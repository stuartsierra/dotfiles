;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; We have to load cider before org-mode to ensure ob-clojure
;; recogines that cider is available:
(require 'cider)

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "stuart.org" user-emacs-directory))

(load-theme 'stuart-default t)
(load-theme 'solarized-dark t)
(load-theme 'stuart-bright-cursor t)

;; Non-disabled commands

(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

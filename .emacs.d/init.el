;; Load my local copy of org-mode, not the version included with Emacs
(push (expand-file-name "vendor/org-mode/lisp" user-emacs-directory) load-path)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "stuart.org" user-emacs-directory))
(put 'erase-buffer 'disabled nil)

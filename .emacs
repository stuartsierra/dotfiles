(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(require 'package)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locally-installed packages (non-ELPA)

(push "~/.emacs.d/clojure-mode" load-path)
(push "~/.emacs.d/cider" load-path)
(push "~/.emacs.d/org-mode/contrib/lisp" load-path)
(push "~/.emacs.d/org-mode/lisp" load-path)
(push "~/.emacs.d/local/" load-path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init

(load "init")

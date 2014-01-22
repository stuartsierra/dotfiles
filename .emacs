(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; Marmalade ELPA repo: http://marmalade-repo.org/
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locally-installed packages (non-ELPA)

(push "~/.emacs.d/magit" load-path)
(push "~/.emacs.d/clojure-mode" load-path)
(push "~/.emacs.d/cider" load-path)
(push "~/.emacs.d/org-mode/contrib/lisp" load-path)
(push "~/.emacs.d/org-mode/lisp" load-path)
(push "~/.emacs.d/local/" load-path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init

(load "init")

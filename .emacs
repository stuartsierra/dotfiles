(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locally-installed packages (non-ELPA)

(push "~/.emacs.d/magit" load-path)
(push "~/.emacs.d/org-mode/contrib/lisp" load-path)
(push "~/.emacs.d/org-mode/lisp" load-path)
(push "~/.emacs.d/local/" load-path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init

(load "init")

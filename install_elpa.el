(setq user-init-file "/tmp/.emacs")

(require 'package)
;; MELPA: http://melpa.org/
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; ;; Marmalade: http://marmalade-repo.org/
;; (add-to-list 'package-archives
;;              '("marmalade" .
;;                "http://marmalade-repo.org/packages/"))
(package-initialize)

(package-refresh-contents)

(defvar my-packages
  '(
    color-theme
    dash
    gh
    gist
    magit
    multiple-cursors
    paredit
    pkg-info
    queue
    ruby-mode
    s
    smex
    xml-rpc
    yasnippet
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

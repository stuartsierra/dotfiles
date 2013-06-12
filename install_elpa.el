(setq user-init-file "/tmp/.emacs")

;; Marmalade: http://marmalade-repo.org/
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(package-refresh-contents)

(package-install 'gist)
(package-install 'paredit)
(package-install 'ruby-mode)
(package-install 'smex)

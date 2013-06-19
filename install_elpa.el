(setq user-init-file "/tmp/.emacs")

;; Marmalade: http://marmalade-repo.org/
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(package-refresh-contents)

(package-install 'color-theme)
(package-install 'css-mode)
(package-install 'gh)
(package-install 'gist)
(package-install 'javascript)
(package-install 'logito)
(package-install 'paredit)
(package-install 'pcache)
(package-install 'ruby-electric)
(package-install 'ruby-mode)
(package-install 'smex)
(package-install 'typopunct)

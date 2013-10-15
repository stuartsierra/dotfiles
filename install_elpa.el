(setq user-init-file "/tmp/.emacs")

;; Marmalade: http://marmalade-repo.org/
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(package-refresh-contents)

(defvar my-packages
  '(color-theme gh gist pkg-info paredit ruby-mode smex typopunct dash))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

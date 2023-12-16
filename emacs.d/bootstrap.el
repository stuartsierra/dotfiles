;;; bootstrap.el

;; Load this file once, in a fresh Emacs configuration,
;; to install the minimal dependencies for the rest of
;; this configuration.

;;; Install use-package

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-refresh-contents)

(package-install 'use-package)

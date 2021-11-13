;;; customizations file

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;;; my custom and vendored elisp files

(eval-and-compile
  (defun my-packages-load-path ()
    (directory-file-name (locate-user-emacs-file "my-packages"))))

;;; use-package initialization

(eval-when-compile
  (require 'use-package))

;;; additional ELPA package archives

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

;;; elisp utilities and dependencies of other packages

;; Needed by other packages, to prevent error
;; "Symbol's value as variable is void: personal-keybindings"
(use-package bind-key
  :ensure t
  :pin melpa-stable)

;; String-manipulation library
(use-package s
  :ensure t
  :pin melpa-stable)

;;; environment and initialization

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :pin melpa-stable
  :config (exec-path-from-shell-initialize))

(use-package rotate
  :ensure t
  :pin melpa)

;;; minibuffer completion

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package ido-vertical-mode
  :ensure t
  :pin melpa-stable
  :config
  (ido-mode 1)
  (ido-vertical-mode 1))

;; Needed to get ido completion in Magit
(use-package ido-completing-read+
  :ensure t
  :pin melpa-stable)

(use-package smex
  :ensure t
  :pin melpa-stable
  :config (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

;;; general-purpose packages

(use-package calc-units
  :after (calc)
  :config
  (setq math-additional-units
      '((PiB "1024 * TiB" "Pebi Byte")
	(TiB "1024 * GiB" "Tebi Byte")
	(GiB "1024 * MiB" "Gibi Byte")
        (MiB "1024 * KiB" "Mebi Byte")
        (KiB "1024 * B" "Kibi Byte")
        (B nil "Byte")
	(Pib "1024 * Tib" "Pebi Bit")
	(Tib "1024 * Gib" "Tebi Bit")
        (Gib "1024 * Mib" "Gibi Bit")
        (Mib "1024 * Kib" "Mebi Bit")
        (Kib "1024 * b" "Kibi Bit")
        (b "B / 8" "Bit")))
  (setq math-units-table nil))

(use-package rg
  :ensure t
  :pin melpa-stable)

(use-package super-save
  :ensure t
  :pin melpa-stable
  :config (super-save-mode +1))

(use-package typo
  :ensure t
  :pin melpa-stable)

;;; project and version-control packages

(use-package find-file-in-project
  :ensure t
  :pin melpa-stable
  :bind ("C-x M-f" . find-file-in-project))

(use-package find-on-github
  :load-path (lambda () (list (my-packages-load-path)))
  :commands (find-on-github browse-on-github))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind ("C-x m" . magit-status))

;;; clojure-related packages

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.bb\\'"
  :interpreter "bb"
  :hook ((clojure-mode . subword-mode)
	 (clojure-mode . hs-minor-mode)
	 (clojure-mode . eldoc-mode)
	 (clojure-mode . flycheck-mode)))

(use-package cider
  :ensure t
  :pin melpa-stable
  :config (setq cider-words-of-inspiration ()))

(use-package paredit
  :ensure t
  :pin melpa-stable
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . clojure-paredit-setup)
	 (emacs-lisp-mode . paredit-mode)))

(use-package lsp-mode
  :ensure t
  :pin melpa-stable
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :ensure t
  :pin melpa-stable
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :pin melpa-stable)

(use-package flycheck
  :ensure t
  ;; Need latest version from MELPA for LSP
  :pin melpa
  :init (global-flycheck-mode))

;;; org-mode and related packages

(use-package org
  :ensure t
  :pin gnu)

(use-package org-contrib
  :ensure t
  :pin nongnu)

;;; server

(when window-system
  (server-start))

;;; local extensions for this machine

(let ((local-config (locate-user-emacs-file "local.el")))
  (when (file-exists-p local-config)
    (load local-config)))

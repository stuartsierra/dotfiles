(package-initialize)

(defun stuart/all-packages-installed-p (packages)
  (cl-loop for pkg in packages
	   when (not (package-installed-p pkg)) do (cl-return nil)
	   finally (cl-return t)))

(defun stuart/install-all-packages (packages)
  (unless (stuart/all-packages-installed-p packages)
    (package-refresh-contents)
    (dolist (pkg packages)
      (when (not (package-installed-p pkg))
	(package-install pkg)))))

(setq stuart/stable-packages
  '(
    ag
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    color-theme-sanityinc-solarized
    exec-path-from-shell
    find-file-in-project
    flycheck-clj-kondo
    gh
    gist
    git-commit
    gnuplot
    go-eldoc
    go-mode
    graphviz-dot-mode
    groovy-mode
    htmlize
    ido-vertical-mode
    inf-clojure
    ledger-mode
    lua-mode
    magit
    markdown-mode
    markdown-toc
    metaweblog  ; org2blog dependency
    multiple-cursors
    org-plus-contrib
    org2blog
    paredit
    rainbow-delimiters
    ruby-mode
    s
    smex
    solarized-theme
    terraform-mode
    typescript-mode
    typo
    visual-fill-column
    xml-rpc
    ))

(setq stuart/unstable-packages
  '(graphql-mode
    hlinum
    linum-off
    ob-restclient
    restclient
    ))

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(stuart/install-all-packages stuart/stable-packages)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(stuart/install-all-packages stuart/unstable-packages)

;; (unless (stuart/all-packages-installed-p)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg stuart/packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))

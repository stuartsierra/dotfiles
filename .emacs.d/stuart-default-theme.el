(deftheme stuart-default
  "Created 2019-12-07.")

(custom-theme-set-variables
 'stuart-default
 '(backup-directory-alist (quote (("." . "/tmp/emacs-backups"))))
 '(backward-delete-char-untabify-method (quote all))
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-help-banner nil)
 '(delete-old-versions t)
 '(dired-listing-switches "-al")
 '(git-commit-summary-max-length 70)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(linum-format "%3d")
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(ns-pop-up-frames nil)
 '(org-adapt-indentation nil)
 '(org-agenda-files (quote ("~/Documents/agenda/")))
 '(org-clock-idle-time 20)
 '(org-clock-into-drawer t)
 '(org-clock-mode-line-total (quote today))
 '(org-confirm-babel-evaluate nil)
 '(org-edit-fixed-width-region-mode (quote fundamental-mode))
 '(org-modules (quote (org-tempo org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup (quote current-window))
 '(org-startup-folded (quote showeverything))
 '(org-yank-folded-subtrees nil)
 '(package-selected-packages (quote (org xml-rpc typescript-mode smex rainbow-delimiters paredit ob-restclient multiple-cursors markdown-mode magit lua-mode linum-off ledger-mode inf-clojure ido-vertical-mode htmlize hlinum groovy-mode graphviz-dot-mode graphql-mode go-eldoc gnuplot gist flycheck-clj-kondo find-file-in-project exec-path-from-shell color-theme-sanityinc-solarized clojure-mode-extra-font-locking cider ag)))
 '(save-interprogram-paste-before-kill t)
 '(sentence-end-double-space nil)
 '(typopunct-buffer-language (quote english))
 '(version-control t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(fringe-mode nil)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward)))

(custom-theme-set-faces
 'stuart-default
 '(default ((t (:height 160 :family "Anonymous Pro")))))

(provide-theme 'stuart-default)

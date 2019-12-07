(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-word-wrap-default-function nil)
 '(backup-directory-alist (quote (("." . "/tmp/emacs-backups"))))
 '(backward-delete-char-untabify-method (quote all))
 '(blink-cursor-mode nil)
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-help-banner nil)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("672f62c9944f34dd276992f1da1554228958e79f33cb65e720703e72e04122da" "f32737ba1638efe480145b27f1c2c7f6676113f6d355ef1a2458084f69959584" default)))
 '(delete-old-versions t)
 '(dired-listing-switches "-alg")
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GROOVY_HOME")))
 '(fringe-mode nil nil (fringe))
 '(git-commit-summary-max-length 70)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(helm-default-external-file-browser "open")
 '(helm-external-programs-associations (quote (("html" . "open") ("pdf" . "open"))))
 '(helm-grep-ag-command "ag --line-numbers -S --color --nogroup %s %s")
 '(image-dired-external-viewer "/usr/bin/open")
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(linum-format "%3d")
 '(magit-commit-all-when-nothing-staged (quote ask-stage))
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(magit-process-popup-time -1)
 '(magit-set-upstream-on-push t)
 '(menu-bar-mode nil)
 '(nrepl-log-messages t)
 '(ns-pop-up-frames nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-adapt-indentation nil)
 '(org-agenda-files (quote ("~/Documents/agenda/")))
 '(org-clock-idle-time 20)
 '(org-clock-into-drawer t)
 '(org-confirm-babel-evaluate nil)
 '(org-edit-fixed-width-region-mode (quote fundamental-mode))
 '(org-export-author-info nil)
 '(org-export-babel-evaluate nil)
 '(org-export-creator-info nil)
 '(org-export-docbook-xsl-fo-proc-command "/opt/local/bin/fop %i %o")
 '(org-export-docbook-xslt-proc-command
   "java -cp \"/opt/local/share/java/*\" net.sf.saxon.Transform -o %o %i %s")
 '(org-export-docbook-xslt-stylesheet "/opt/local/share/xsl/docbook-xsl/fo/docbook.xsl")
 '(org-export-html-validation-link "")
 '(org-export-time-stamp-file nil)
 '(org-export-with-author nil)
 '(org-export-with-section-numbers nil)
 '(org-export-with-tags nil)
 '(org-export-with-toc nil)
 '(org-html-htmlize-output-type (quote css))
 '(org-html-validation-link "")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup (quote current-window))
 '(org-startup-folded (quote showeverything))
 '(org-tag-alist
   (quote
    (("billable" . 98)
     ("nonbillable" . 110)
     ("twentypercent" . 116)
     ("offclock" . 111)
     ("product" . 112))))
 '(org-todo-keyword-faces
   (quote
    (("INPROGRESS" . "cyan")
     ("STARTED" . "cyan")
     ("BLOCKED" . "red")
     ("WAITING" . "orange"))))
 '(org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE"))))
 '(org-yank-folded-subtrees nil)
 '(save-interprogram-paste-before-kill t)
 '(sentence-end-double-space nil)
 '(slime-kill-without-query-p t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(slime-truncate-lines nil)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(typopunct-buffer-language (quote english))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(visual-line-mode nil t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :family "Anonymous Pro")))))

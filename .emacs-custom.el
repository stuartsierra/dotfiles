;; for compatibility with older Aquamacs versions
 (defvar aquamacs-140-custom-file-upgraded t)
 (unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-autoface-mode nil)
 '(aquamacs-customization-version-id 208 t)
 '(aquamacs-tool-bar-user-customization
   (quote
    ((16777249 new-file open-file recent-files save-buffer aquamacs-print nil undo redo cut copy paste isearch-forward nil customize help))) t)
 '(auto-word-wrap-default-function nil)
 '(backup-directory-alist (quote (("." . "/tmp/emacs-backups"))))
 '(backward-delete-char-untabify-method (quote all))
 '(column-number-mode t)
 '(delete-old-versions t)
 '(dired-listing-switches "-alg")
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GROOVY_HOME")))
 '(fringe-mode nil nil (fringe))
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
 '(org-agenda-files
   (quote
    ("~/Documents/agenda/relevance.org" "~/Documents/agenda/cognitect.org" "~/Documents/agenda/personal.org" "~/Documents/agenda/other/")))
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

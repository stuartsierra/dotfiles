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
 '(aquamacs-tool-bar-user-customization (quote ((16777249 new-file open-file recent-files save-buffer aquamacs-print nil undo redo cut copy paste isearch-forward nil customize help))) t)
 '(backward-delete-char-untabify-method (quote all))
 '(dired-listing-switches "-alg")
 '(fringe-mode nil nil (fringe))
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(linum-format "%3d ")
 '(magit-commit-all-when-nothing-staged (quote ask-stage))
 '(menu-bar-mode nil)
 '(nrepl-popup-stacktraces nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-agenda-files (quote ("~/Documents/agenda/relevance.org" "~/Documents/agenda/personal.org" "~/Documents/agenda/other/")))
 '(org-export-author-info nil)
 '(org-export-babel-evaluate nil)
 '(org-export-creator-info nil)
 '(org-export-docbook-xsl-fo-proc-command "/opt/local/bin/fop %i %o")
 '(org-export-docbook-xslt-proc-command "java -cp \"/opt/local/share/java/*\" net.sf.saxon.Transform -o %o %i %s")
 '(org-export-docbook-xslt-stylesheet "/opt/local/share/xsl/docbook-xsl/fo/docbook.xsl")
 '(org-export-html-validation-link "")
 '(org-export-htmlize-output-type (quote css))
 '(org-export-time-stamp-file nil)
 '(org-special-ctrl-a/e t)
 '(org-startup-folded (quote showeverything))
 '(org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE"))))
 '(safe-local-variable-values (quote ((eval setq org-export-htmlize-output-type (quote css)) (buffer-file-coding-system . utf-8-unix) (org-export-html-style-include-scripts) (eval define-clojure-indent (to-data (quote defun))))))
 '(sentence-end-double-space nil)
 '(slime-kill-without-query-p t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(slime-truncate-lines nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visual-line-mode nil t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:height 200 :family "Anonymous_Pro"))))
 '(font-lock-warning-face ((t (:background "#EE799F" :foreground "black"))))
 '(fringe ((((type ns)) (:background "#141314" :foreground "grey55"))))
 '(linum-highlight-face ((t (:inherit linum-face :foreground "yellow")))))

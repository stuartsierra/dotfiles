
;; for compatibility with older Aquamacs versions
 (defvar aquamacs-140-custom-file-upgraded t)
 (unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))
(put 'erase-buffer 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 208 t)
 '(aquamacs-tool-bar-user-customization (quote ((16777249 new-file open-file recent-files save-buffer aquamacs-print nil undo redo cut copy paste isearch-forward nil customize help))) t)
 '(default-frame-alist (quote ((menu-bar-lines . 1) (foreground-color . "Black") (background-color . "White") (cursor-type . box) (cursor-color . "Red") (vertical-scroll-bars . right) (internal-border-width . 0) (left-fringe . 1) (right-fringe) (fringe))))
 '(global-auto-revert-mode t)
 '(global-linum-mode t)
 '(linum-format "%3d ")
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE"))))
 '(safe-local-variable-values (quote ((eval define-clojure-indent (to-data (quote defun))))))
 '(slime-kill-without-query-p t)
 '(slime-net-coding-system (quote utf-8-unix))
 '(slime-truncate-lines nil)
 '(tabbar-mode nil nil (tabbar))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visual-line-mode nil t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :foundry "Anonymous_Pro"))))
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "#444")))))

(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

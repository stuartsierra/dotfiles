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
 '(global-auto-revert-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(linum-format "%3d ")
 '(menu-bar-mode nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-special-ctrl-a/e t)
 '(org-startup-folded (quote showeverything))
 '(org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE"))))
 '(safe-local-variable-values (quote ((eval define-clojure-indent (to-data (quote defun))))))
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
 '(default ((t (:stipple nil :background "#141414" :foreground "#CACACA" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :family "Anonymous Pro"))))
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "#444"))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "green"))))
 '(mode-line-inactive ((t (:inherit aquamacs-variable-width :background "grey10" :foreground "grey40" :box (:line-width -1 :color "grey20") :strike-through nil :underline nil :slant normal :weight normal :width normal))))
 '(org-hide ((((background dark)) (:foreground "#141414"))))
 '(outline-4 ((t (:foreground "#8F8A80")))))

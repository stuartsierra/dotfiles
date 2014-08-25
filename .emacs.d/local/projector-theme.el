(deftheme projector
  "Created 2013-08-14.")

(custom-theme-set-faces
 'projector
 '(highlight ((t (:background "darkseagreen2"))))
 '(font-lock-builtin-face ((t (:foreground "dark slate blue"))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "Firebrick"))))
 '(font-lock-constant-face ((t (:foreground "purple"))))
 '(font-lock-doc-face ((t (:foreground "DarkGreen"))))
 '(font-lock-function-name-face ((t (:foreground "Blue"))))
 '(font-lock-keyword-face ((t (:foreground "dark slate blue"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold))))
 '(font-lock-string-face ((t (:foreground "DarkGreen"))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit error))))
 '(org-code ((t (:inherit bold))))
 '(org-block-begin-line ((t (:foreground "Gray"))))
 '(org-block-end-line ((t (:foreground "Gray"))))
 '(org-meta-line ((t (:foreground "Gray"))))
 '(org-block-background ((t :background "#eee")))
 '(default ((t (:background "white" :foreground "black")))))

(provide-theme 'projector)

;;; ruby-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ruby-mode) "ruby-mode" "ruby-mode.el" (19829 40053))
;;; Generated autoloads from ruby-mode.el

(autoload (quote ruby-mode) "ruby-mode" "\
Major mode for editing Ruby scripts.
\\[ruby-indent-line] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.rb$" . ruby-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("ruby" . ruby-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("rbx" . ruby-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("jruby" . ruby-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("ruby1.9" . ruby-mode)))

(add-to-list (quote interpreter-mode-alist) (quote ("ruby1.8" . ruby-mode)))

;;;***

;;;### (autoloads nil nil ("ruby-mode-pkg.el") (19829 40053 99797))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ruby-mode-autoloads.el ends here

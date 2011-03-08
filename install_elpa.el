(let ((buffer (url-retrieve-synchronously
	       "http://tromey.com/elpa/package-install.el")))
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (eval-region (point) (point-max))
    (kill-buffer (current-buffer))))

(package-refresh-contents)

(package-install 'css-mode)
(package-install 'gist)
(package-install 'javascript)
(package-install 'paredit)
(package-install 'ruby-mode)
(package-install 'slime)
(package-install 'slime-repl)
(package-install 'smex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-file-in-project

(require 'find-file-in-project)
(global-set-key (kbd "C-x M-f") 'find-file-in-project)

(push "*.edn" ffip-patterns)
(push "*.dtm" ffip-patterns)
(push "*.xml" ffip-patterns)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired

;; From https://github.com/candera/emacs/blob/master/init.el
(defun dired-insert-this-directory-recursively ()
  "Recursively insert the subdirectories of the current dired directory."
  (interactive)
  (dired-insert-subdir dired-directory "-alR"))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "I") 'dired-insert-this-directory-recursively)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;; http://github.com/philjackson/magit/downloads

(require 'magit)
(global-set-key (kbd "C-x m") 'magit-status)

;;; following from https://github.com/magnars/.emacs.d/blob/master/setup-magit.el

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE ASSOCIATIONS

(add-to-list 'auto-mode-alist '("\\.\\(rdfs?\\|owl\\)$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.st$" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.\\(cljs?\\|dtm\\|edn\\)$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUBY

;; Missing from ruby-mode.el, see https://groups.google.com/group/emacs-on-rails/msg/565fba8263233c28
(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-electric)
            (ruby-electric-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LINUX-STYLE C CODE

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8))

(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
                       auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVA

(defun my-electric-brace ()
  (interactive)
  (insert " {")
  (backward-char)
  (fixup-whitespace)
  (move-end-of-line 1)
  (indent-for-tab-command)
  (insert "\n\n")
  (insert "}")
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))

(eval-after-load 'cc-mode
  '(define-key java-mode-map (kbd "{") 'my-electric-brace))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-on-github

(require 'find-on-github)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO
;; http://www.emacswiki.org/emacs/InteractivelyDoThings

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMEX
;; http://github.com/nonsequitur/smex/

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode and Day Pages
;; http://almostobsolete.net/daypage.html

(require 'org)
(require 'org-clock)
(require 'org-faces)

(add-hook 'org-mode-hook 'auto-fill-mode)

(set-face-attribute 'org-mode-line-clock nil :background "pink")

(when (fboundp 'set-word-wrap)
  (add-hook 'org-mode-hook 'set-word-wrap))

(setq daypage-path "~/Documents/daypage/")

(defun find-daypage (&optional date)
  "Go to the day page for the specified date,
   or toady's if none is specified."
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (setq date (or date (current-time)))
  (let* ((file (expand-file-name
                (concat daypage-path
                        (format-time-string "daypage-%Y-%m-%d-%a" date) ".org")))
         (buffer (find-buffer-visiting file)))
    (if buffer
        (pop-to-buffer buffer)
      (find-file file))
    (when (= 0 (buffer-size))
      ;; Insert an initial for the page
      (insert (format-time-string "%Y-%m-%d %A : " date)))))

(defun todays-daypage ()
  "Go straight to today's day page without prompting for a date."
  (interactive)
  (find-daypage))

(global-set-key "\C-con" 'todays-daypage)
(global-set-key "\C-coN" 'find-daypage)

(defun org-to-confluence ()
  "Convert region from org-mode to Confluence wiki syntax"
  (interactive)
  (save-excursion
   (let ((start (region-beginning))
         (end (region-end)))
     (replace-regexp "^\\*\\*\\* \\(.+\\)$" "\nh2. \\1\n" nil start end)
     (replace-regexp "^\\*\\*\\*" "" nil start end))))

(defun my-agenda ()
  (interactive)
  (org-agenda nil "n"))

(global-set-key (kbd "C-c o a") 'my-agenda)

(defun org-fixup-buffer-whitespace ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "^\\*+" (point-max) t)
    (beginning-of-line)
    (newline)
    (forward-line 1)
    (newline)))

(setq org-default-src-language "clojure")

(defun org-insert-src-block ()
  (interactive)
  (insert "#+BEGIN_SRC "
          org-default-src-language
          "\n\n#+END_SRC\n")
  (previous-line 2)
  (org-edit-special))

(define-key org-mode-map (kbd "C-c o s") 'org-insert-src-block)

(require 'org-element)
;; Modified from org-element.el to add support for ` as a markup
;; character, after customizing org-emphasis-alist.
(defun org-element-text-markup-successor (limit)
  "Search for the next text-markup object.

LIMIT bounds the search.

Return value is a cons cell whose CAR is a symbol among `bold',
`italic', `underline', `strike-through', `code' and `verbatim'
and CDR is beginning position."
  (save-excursion
    (unless (bolp) (backward-char))
    (when (re-search-forward org-emph-re limit t)
      (let ((marker (match-string 3)))
	(cons (cond
	       ((equal marker "*") 'bold)
	       ((equal marker "/") 'italic)
	       ((equal marker "_") 'underline)
	       ((equal marker "+") 'strike-through)
	       ((equal marker "~") 'code)
	       ((equal marker "=") 'verbatim)
	       ((equal marker "`") 'code)
	       (t (error "Unknown marker at %d" (match-beginning 3))))
	      (match-beginning 2))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE/LISP/NREPL/CIDER

(require 'cider)
(require 'clojure-mode)
(require 'paredit)
(require 'align-cljlet)

(show-paren-mode 1)

(defun clojure-paredit-hook () (paredit-mode +1))
(add-hook 'clojure-mode-hook 'clojure-paredit-hook)

(define-key clojure-mode-map (kbd "C-c M-k") 'cider-copy-current-ns)
(define-key clojure-mode-map "{" 'paredit-open-brace)
(define-key clojure-mode-map "}" 'paredit-close-brace)

(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-[") nil)

;; Custom indentation rules; see clojure-indent-function
(define-clojure-indent
  (for-all 'defun)
  (describe 'defun)
  (testing 'defun)
  (given 'defun)
  (using 'defun)
  (with 'defun)
  (it 'defun)
  (do-it 'defun)
  (go-loop 'defun))

(setq cider-words-of-inspiration '(""))

(global-set-key (kbd "C-c o r") 'cider-switch-to-repl-buffer)

(setq inferior-lisp-program "~/bin/clj")

(defun cider-copy-current-ns ()
  "Copies the name of the current Clojure namespace to the kill
ring."
  (interactive)
  (let ((ns (cider-current-ns)))
    (kill-new ns)
    (message ns)))

(defun cider-execute-in-current-repl (expr)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert expr)
    (cider-repl-return)))

(defun clj-file-p ()
  (string-match "\.clj$" (buffer-file-name)))

(defun cider-refresh ()
  (interactive)
  (save-some-buffers t 'clj-file-p)
  (cider-execute-in-current-repl
   "(clojure.tools.namespace.repl/refresh)"))

(defun cider-reset ()
  (interactive)
  (save-some-buffers t 'clj-file-p)
  (cider-execute-in-current-repl
   "(user/reset)"))

(defun cider-eval-register-in-repl (register)
  (interactive "cEval register in CIDER REPL: ")
  (cider-execute-in-current-repl (get-register register)))

(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (cider-execute-in-current-repl form)))

(defun cider-run-tests ()
  (interactive)
  (cider-execute-in-current-repl
   (or (get-register ?t) "(clojure.test/run-tests)")))

(defun cider-clear-repl-buffer ()
  (interactive)
  (if (not (get-buffer (cider-current-connection-buffer)))
      (message "No active cider connection.")
    (progn
      (set-buffer (cider-find-or-create-repl-buffer))
      (cider-clear-buffer))))

(global-set-key (kbd "s-t") 'cider-run-tests)
(global-set-key (kbd "s-1") '(lambda () (interactive) (cider-eval-register-in-repl ?1)))
(global-set-key (kbd "s-2") '(lambda () (interactive) (cider-eval-register-in-repl ?2)))
(global-set-key (kbd "s-3") '(lambda () (interactive) (cider-eval-register-in-repl ?3)))
(global-set-key (kbd "s-4") '(lambda () (interactive) (cider-eval-register-in-repl ?4)))
(global-set-key (kbd "s-5") '(lambda () (interactive) (cider-eval-register-in-repl ?5)))
(global-set-key (kbd "s-r") 'cider-refresh)
(global-set-key (kbd "s-R") 'cider-reset)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel + Clojure

(load "org-babel-cider")

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

;; Avoid slow "Fontifying..." on OS X
(setq font-lock-verbose nil)

(defun org-babel-execute-in-repl ()
  (interactive)
  (let ((body (cadr (org-babel-get-src-block-info))))
    (cider-execute-in-current-repl body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Text Manipulation

;; from http://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
  logical line. This is useful, e.g., for use with
  `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun zap-up-to-char (arg char)
  "Kill up to but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap up to char: ")
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
                         (search-forward (char-to-string char) nil nil arg)
;                        (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
                         (1- (point))))
  (goto-char (1- (point))))

(global-unset-key "\M-z")
(global-set-key "\M-z" 'zap-up-to-char)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create temporary buffer

(defun temp-buffer ()
  (interactive)
  (switch-to-buffer "*temp*"))

(global-set-key (kbd "C-c o t") 'temp-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-numbering

(require 'linum-off)
(require 'hlinum)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace

;; from https://github.com/candera/emacs/blob/3cc572daf3148a1aebe2fc69c1c93e462dba2fee/init.el#L298

(defun detabify-buffer ()
  "Calls untabify on the current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defvar detabify-modes '(javascript-mode emacs-lisp-mode ruby-mode clojure-mode java-mode)
  "A list of the modes that will have tabs converted to spaces before saving.")

(defun mode-aware-detabify ()
  "Calls untabify on the current buffer if the major mode is one of 'detabify-modes'"
  (interactive)
  (when (member major-mode detabify-modes)
    (detabify-buffer)))

(defvar delete-trailing-whitespace-modes detabify-modes
  "A list of the modes that will have trailing whitespace before saving.")

(defun mode-aware-trailing-whitespace-cleanup ()
  "Calls delete-trailing-whitespace-modes on the current buffer
if the major mode is one of 'delete-trailing-whitespace-modes'"
  (interactive)
  (when (member major-mode delete-trailing-whitespace-modes)
    (delete-trailing-whitespace)))

(defun clean-up-whitespace ()
  "Calls untabify and delete-trailing-whitespace on the current buffer."
  (interactive)
  (detabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-x t") 'clean-up-whitespace)

(defun toggle-show-whitespace ()
  (interactive)
  (setq show-trailing-whitespace
        (not show-trailing-whitespace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window (pane) management

;; from http://emacswiki.org/emacs/TransposeWindows
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(global-set-key (kbd "s-}") 'other-window)
(global-set-key (kbd "s-{") (lambda () (interactive) (other-window -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typopunct mode

;; http://www.emacswiki.org/emacs/TypographicalPunctuationMarks

(require 'typopunct)

(defconst typopunct-ellipsis (decode-char 'ucs #x2026))
(defconst typopunct-middot   (decode-char 'ucs #xB7)) ; or 2219
(defun typopunct-insert-ellipsis-or-middot (arg)
  "Change three consecutive dots to a typographical ellipsis mark."
  (interactive "p")
  (cond
   ((and (= 1 arg)
         (eq (char-before) ?^))
    (delete-char -1)
    (insert typopunct-middot))
   ((and (= 1 arg)
         (eq this-command last-command)
         (looking-back "\\.\\."))
    (replace-match "")
    (insert typopunct-ellipsis))
   (t
    (self-insert-command arg))))
(define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text scaling (zoom)

(defun text-scale-reset ()
  "Disables text scaling (zoom)"
  (interactive)
  (text-scale-set 0))

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-reset)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s-_") 'text-scale-decrease)
(global-set-key (kbd "s-)") 'text-scale-reset)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server

(server-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Twilight" color theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/local")
(load-theme 'twilight-stuart t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs / Cocoa Emacs stuff

(when (fboundp 'tabbar-mode) (tabbar-mode -1))

(when (boundp 'osx-key-mode-map)
  (define-key osx-key-mode-map (kbd "C-;") nil))

;; from https://gist.github.com/1297644
(defun finder (location)
  "Fire up finder in a location relative to pwd."
  (interactive "sOpen finder at this location (relative to pwd): ")
  (start-process "finder" "finder" "open" "-a" "Finder.app" location))

;; Has to come late in the initialization process
(when (display-graphic-p)
  (menu-bar-mode 1))

;; Fix Unicode character spacing; see http://stackoverflow.com/q/8779351
(when (string-equal system-type "darwin")
  (set-fontset-font "fontset-default"
                    'unicode
                    '("Menlo" . "iso10646-1")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Executable $PATH from ~/.path

(defun slurp (path)
  "Return file's contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(let ((path (slurp "~/.path")))
  (mapcar (lambda (s)
            (if (zerop (length s))
                nil
              (progn (push s exec-path)
                     (setenv "PATH" (concat (getenv "PATH") ":" s)))))
          (split-string path "\n")))

;; Work around path bug on OS X
(when (string-equal "/" default-directory)
  (cd "~/"))

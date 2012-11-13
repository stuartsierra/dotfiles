(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locally-installed packages (non-ELPA)

(push "~/.emacs.d/local/" load-path)
(push "~/.emacs.d/local/org-mode/lisp" load-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs / Cocoa Emacs stuff

(when (fboundp 'tabbar-mode) (tabbar-mode -1))

(when (boundp 'osx-key-mode-map)
  (define-key osx-key-mode-map (kbd "C-;") nil))

(when (fboundp 'fringe-mode) (fringe-mode 0))

;; from https://gist.github.com/1297644
(defun finder (location)
  "Fire up finder in a location relative to pwd."
  (interactive "sOpen finder at this location (relative to pwd): ")
  (start-process "finder" "finder" "open" "-a" "Finder.app" location))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;; http://github.com/philjackson/magit/downloads

(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x m") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE/SWANK/SLIME

(show-paren-mode 1)

(autoload 'clojure-mode "clojure-mode" nil t)

(eval-after-load 'clojure-mode
  '(progn
     (require 'paredit)
     (defun clojure-paredit-hook () (paredit-mode +1))
     (add-hook 'clojure-mode-hook 'clojure-paredit-hook)
     
     (define-key clojure-mode-map "{" 'paredit-open-brace)
     (define-key clojure-mode-map "}" 'paredit-close-brace)
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-[") nil)

     ;; Custom indentation rules; see clojure-indent-function
     (define-clojure-indent
       (describe 'defun)
       (testing 'defun)
       (given 'defun)
       (using 'defun)
       (with 'defun)
       (it 'defun)
       (do-it 'defun))))

(eval-after-load 'slime
  '(setq slime-protocol-version 'ignore))

(require 'elein)

(setq inferior-lisp-program "~/bin/clj")

(defun my-slime-repl ()
  (interactive)
  (if (slime-connected-p)
      (slime-repl)
    (slime-connect "localhost" 4005)))

(global-set-key (kbd "C-c o r") 'my-slime-repl)

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
  (find-file 
       (expand-file-name 
        (concat daypage-path 
        (format-time-string "daypage-%Y-%m-%d-%a" date) ".org")))
  (when (= 0 (buffer-size))
        ;; Insert an initial for the page
        (insert (format-time-string "%Y-%m-%d %A : " date))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel + Clojure

(require 'nrepl)

(when (locate-file "ob" load-path load-suffixes)
  (require 'ob)
  (require 'ob-tangle)
  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)))

  (defun org-babel-execute:clojure (body params)
    "Evaluate a block of Clojure code with Babel."
    (let* ((result (nrepl-send-string-sync body (nrepl-current-ns)))
           (value (plist-get result :value))
           (out (plist-get result :stdout))
           (out (when out
                  (if (string= "\n" (substring out -1))
                      (substring out 0 -1)
                    out)))
           (stdout (when out
                     (mapconcat (lambda (line)
                                  (concat ";; " line))
                                (split-string out "\n")
                                "\n"))))
      (concat stdout
              (when (and stdout (not (string= "\n" (substring stdout -1))))
                "\n")
              ";;=> " value)))

  (provide 'ob-clojure)

  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil))

;; Avoid slow "Fontifying..." on OS X
(setq font-lock-verbose nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zap-up-to-char

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
;			 (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
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
;; Rotate windows

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Twilight" color theme

(load-library "color-theme-twilight")

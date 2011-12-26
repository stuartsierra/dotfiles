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

(autoload 'clojure-mode "clojure-mode" nil t)

(eval-after-load 'clojure-mode
  '(progn
     (require 'paredit)
     (defun clojure-paredit-hook () (paredit-mode +1))
     (add-hook 'clojure-mode-hook 'clojure-paredit-hook)
     
     (define-key clojure-mode-map "{" 'paredit-open-brace)
     (define-key clojure-mode-map "}" 'paredit-close-brace)
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)

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

(defun clojure-repl ()
  (interactive)
  (inferior-lisp "java -jar /Users/stuart/src/clojure/clojure/target/clojure-1.3.0-master-SNAPSHOT.jar"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE ASSOCIATIONS

(add-to-list 'auto-mode-alist '("\\.\\(rdfs?\\|owl\\)$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.st$" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))
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

(add-hook 'org-mode-hook 'set-word-wrap)

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
        (insert (format-time-string "* %Y-%m-%d %A : " date))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel + Clojure

(require 'ob)
(require 'ob-tangle)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure 
 '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
 "Evaluate a block of Clojure code with Babel."
 (lisp-eval-string body)
 "Done!")

(provide 'ob-clojure)

(setq org-src-fontify-natively nil)
(setq org-confirm-babel-evaluate nil)


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
;; Line-numbering

(require 'hlinum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Twilight" color theme

(load-library "color-theme-twilight")

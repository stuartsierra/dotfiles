;;; find-on-github.el --- Open current file on GitHub.com

;; Author: Stuart Sierra <mail@stuartsierra.com>
;; Keywords: git, github, browse, url
;; URL: https://github.com/stuartsierra/dotfiles

;; This file is *NOT* part of GNU Emacs.

;; The MIT License (MIT)

;; Copyright (c) 2013 Stuart Sierra

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Call browse-on-github to open a web browser showing the current
;; file, at the current commit, on GitHub.com.  This assumes that the
;; file is in a Git repository whose "remote.origin.url" property
;; points to github.com.  Works for both public (https://github.com)
;; and private (git@github.com) repository URLs.

;; Inspired by https://github.com/gleitz/browse-on-github.el but
;; rewritten without an external script.

;;; Code:

(defun call-git (&rest args)
  "Invoke git with command line ARGS and return output.
Returns a string of whatever the git command prints to STDOUT,
whitespace-trimmed."
  (cl-labels ((strip-whitespace (string)
                (let ((s string))
                  (while (string-match "\\`\s+\\|\n+\\'" s)
                    (setq s (replace-match "" t t s)))
                  s)))
    (with-temp-buffer
      (if (equal 0 (apply 'call-process "git" nil t nil args))
          (strip-whitespace (buffer-string))
        (error "Git failed")))))

(defun git-remote-to-web-url (remote-url)
  "Convert Git remote origin REMOTE-URL into a GitHub web page URL."
  (let ((url remote-url))
    (when (string-match "^git@github\\.com:" url)
      (setq url (replace-match "https://github.com/" t t url)))
    (when (string-match "^git://github\\.com/" url)
      (setq url (replace-match "https://github.com/" t t url)))
    (when (string-match "\\.git$" url)
      (setq url (replace-match "" t t url)))
    url))

(defun github-hash-fragment ()
  "Return GitHub URL hash fragment for current region or point.
Like #L4-10 if region covers lines 4 through 10, or nil if point
is at the beginning of the buffer."
  (cond ((region-active-p)
         (concat "#L" (int-to-string (line-number-at-pos (region-beginning)))
                 "-L" (int-to-string
                       (let ((line (line-number-at-pos (region-end))))
                         (if (= 10 (char-before (region-end))) ; empty line
                             (1- line)
                           line)))))
        ((= (point-min) (point))
         nil)
        (t (concat "#L" (int-to-string (line-number-at-pos (point)))))))

(defun current-git-commit ()
  "Return commit hash of the current Git commit."
  (call-git "rev-parse" "HEAD"))

(defun github-url (branch-or-commit)
  "Return a GitHub URL for current file at BRANCH-OR-COMMIT.
If region is active, selects the region of lines.  Otherwise
selects the line at point."
  (let* ((remote-url (call-git "config" "--get" "remote.origin.url"))
         (web-url-base (git-remote-to-web-url remote-url))
         (relative-path (call-git "rev-parse" "--show-prefix"))
         (file-name (when (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name)))))
    (concat web-url-base
            (if file-name "/blob/" "/tree/")
            branch-or-commit "/" relative-path file-name
            (when file-name (github-hash-fragment)))))

(defun find-on-github (branch-or-commit)
  "Show GitHub URL for current file at BRANCH-OR-COMMIT.
The URL will highlight the current line or region.  Also copies
the URL to the kill ring."
  (interactive
   (if current-prefix-arg
       (list (read-string "Git branch (default master): " nil nil "master"))
     (list (current-git-commit))))
  (let ((url (github-url branch-or-commit)))
    (kill-new url)
    (message url)))

(defun browse-on-github (branch-or-commit)
  "Open web browser to current file at BRANCH-OR-COMMIT in GitHub.
Also displays the URL and copies it to the kill ring."
  (interactive
   (if current-prefix-arg
       (list (read-string "Git branch (default master): " nil nil "master"))
     (list (current-git-commit))))
  (let ((url (github-url branch-or-commit)))
    (kill-new url)
    (message url)
    (browse-url url)))

(provide 'find-on-github)

;;; find-on-github.el ends here

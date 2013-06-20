;;; find-on-github.el --- Open current file on GitHub.com

;; Author: Stuart Sierra <mail@stuartsierra.com>
;; Keywords: git, github, browse, url
;; URL: https://github.com/gstamp/align-cljlet


;; This file is *NOT* part of GNU Emacs.

;; Copyright (c) 2013 Stuart Sierra. All rights reserved. This program
;; and the accompanying materials are made available under the terms
;; of the Eclipse Public License v1.0 which accompanies this
;; distribution, and is available at
;; http://www.eclipse.org/legal/epl-v10.html


;; Call browse-on-github to open a web browser showing the current
;; file, at the current commit, on GitHub.com. This assumes that the
;; file is in a Git repository whose "remote.origin.url" property
;; points to github.com. Works for both public (https://github.com)
;; and private (git@github.com) repository URLs.


(defun call-git
  "Invoke git with the given arguments and return whatever it
  prints to STDOUT, whitespace-trimmed."
  (&rest args)
  (cl-labels ((strip-whitespace (string)
                (let ((s string))
                  (while (string-match "\\`\s+\\|\n+\\'" s)
                    (setq s (replace-match "" t t s)))
                  s)))
    (with-temp-buffer
      (if (equal 0 (apply 'call-process "git" nil t nil args))
          (strip-whitespace (buffer-string))
        (error "Git failed")))))

(defun git-remote-to-web-url
  "Convert a Git remote origin URL into a GitHub web page URL."
  (remote-url)
  (let ((url remote-url))
    (when (string-match "^git@github\\.com:" url)
      (setq url (replace-match "https://github.com/" t t url)))
    (when (string-match "\\.git$" url)
      (setq url (replace-match "" t t url)))
    url))

(defun github-hash-fragment
  "Returns a URL hash fragment like #L4-10 based on point and/or
  region in the current buffer. Returns nil if point is at the
  beginning of the buffer."
  ()
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

(defun github-url ()
  "Returns a GitHub URL for viewing the current file at the
  current commit. If region is active, selects the region of
  lines. Otherwise selects the line at point."
  (let* ((remote-url (call-git "config" "--get" "remote.origin.url"))
         (web-url-base (git-remote-to-web-url remote-url))
         (commit (call-git "rev-parse" "HEAD"))
         (relative-path (call-git "rev-parse" "--show-prefix"))
         (file-name (when (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name)))))
    (concat web-url-base
            (if file-name "/blob/" "/tree/")
            commit "/" relative-path file-name
            (when file-name (github-hash-fragment)))))

(defun find-on-github ()
  "If the current buffer is showing a file or directory in a Git
  repository, displays a URL for it on github.com with the
  current line or region highlighted. Also copies the URL to the
  kill ring."
  (interactive)
  (let ((url (github-url)))
    (kill-new url)
    (message url)))

(defun browse-on-github ()
  "If the current buffer is showing a file or directory in a Git
  repository, opens a web browser showing that file on github.com
  at the current commit, with the current line or region
  highlighted. Also displays the URL and copies it to the kill
  ring."
  (interactive)
  (let ((url (github-url)))
    (kill-new url)
    (message url)
    (browse-url url)))


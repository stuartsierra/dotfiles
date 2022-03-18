;;; daypage --- daily work journal in org mode

;; Author: Stuart Sierra <mail@stuartsierra>
;; URL: https://github.com/stuartsierra/dotfiles

;; MIT License

;; Copyright (c) 2021 Stuart Sierra

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

;; Inspired by https://almostobsolete.net/daypage.html

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-faces)

(defvar stuart/daypage-path "~/Documents/daypage/")

(defvar stuart/daypage-default-project nil)
(defvar stuart/daypage-default-tags nil)

(defun stuart/daypage-filename-base (date)
  "Return daypage filename (without extension) for DATE."
  (format-time-string "daypage-%Y-%m-%d-%a" date))

(defun stuart/find-daypage (&optional date)
  "Open journal page for DATE, default today."
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (setq date (or date (current-time)))
  (let* ((file (expand-file-name
                (concat stuart/daypage-path
                        (stuart/daypage-filename-base date) ".org")))
         (buffer (find-buffer-visiting file)))
    (if buffer
        (switch-to-buffer buffer)
      (find-file file))
    (when (= 0 (buffer-size))
      (let ((datestr (format-time-string "#+TITLE: %Y-%m-%d %A" date)))
        ;; Insert an initial heading for the page
        (insert datestr)
        (when stuart/daypage-default-project
          (insert " : " stuart/daypage-default-project "\n\n")
          (insert "* " stuart/daypage-default-project)
          (when stuart/daypage-default-tags
            (org-set-tags stuart/daypage-default-tags)))))))

(defun stuart/todays-daypage ()
  "Go straight to today's day page without prompting for a date."
  (interactive)
  (stuart/find-daypage))

(defun stuart/copy-daypage-dir (&optional date)
  "Copy directory for day page DATE, default today."
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (setq date (or date (current-time)))
  (let ((dir (stuart/daypage-filename-base date)))
    (kill-new dir)
    (message dir)))

(defun stuart/find-daypage-dir (&optional date)
  "Open subdirectory for day page DATE, default today."
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (setq date (or date (current-time)))
  (let* ((dir (expand-file-name
               (concat stuart/daypage-path
                       (stuart/daypage-filename-base date))))
         (buffer (find-buffer-visiting dir)))
    (mkdir dir t)
    (if buffer
        (switch-to-buffer buffer)
      (find-file dir))))

(provide 'daypage)

;;; daypage.el ends here

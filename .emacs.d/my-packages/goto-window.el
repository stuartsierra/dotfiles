;;; goto-window.el --- Switch windows forward and backward

;; Author: Stuart Sierra <mail@stuartsierra.com>
;; Keywords: git, github, browse, url
;; URL: https://github.com/stuartsierra/dotfiles

;; This file is *NOT* part of GNU Emacs.

;; The MIT License (MIT)

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

;; Defined functions so I can bind them to hotkeys, like switching
;; tabs in a browser.

;;; Code:

(defun stuart/goto-window-next ()
  "Switch to next window."
  (interactive)
  (other-window 1))

(defun stuart/goto-window-prev ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(provide 'goto-window)

;;; goto-window.el ends here

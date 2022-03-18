;;; link-cider-session.el --- Link any buffer to a running CIDER session

;; Author: Stuart Sierra <mail@stuartsierra.com>
;; Keywords: cider, sesman, clojure

;; This file is *NOT* part of GNU Emacs.

;; The MIT License (MIT)

;; Copyright (c) 2022 Stuart Sierra

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

;; CIDER and Sesman associate buffers with REPL processes based on
;; project directories.  I often work in an org-mode file in a
;; different directory tree from the project REPL, but I still want to
;; use org-babel to execute Clojure code inline.

;;; Code:

(require 'cider)
(require 'sesman)

(defun link-cider-session ()
  "Link the current buffer to a running CIDER session."
  (interactive)
  (setq sesman-system 'CIDER)
  (sesman-link-with-buffer))

(provide 'link-cider-session)

;;; link-cider-session.el ends here

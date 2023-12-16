;;; org-copy-for-external --- Copy org elements to clipboard for other apps

;; Author: Alessandra Sierra <info@lambdasierra.com>
;; Keywords: org
;; URL: https://github.com/stuartsierra/dotfiles

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

;; Use org-copy-for-external to copy the current org element to the
;; clipboard in a way that is convenient for pasting into other
;; external applications.

;; For example, if you have a source code block in an org file, you
;; can evaluate it with org-babel, but you may also want to paste it
;; into a terminal. org-copy-for-external will copy just the contents
;; of the source block, stripping extra whitespace and indentation.

;;; Code:

(require 'org-element)

(defun org-copy-for-external ()
  "Copy current org element for pasting in external applications.

Copies contents of source code, example, and fixed-width blocks."
  (interactive)
  (let* ((element (org-element-at-point))
         (element-type (org-element-type element)))
    (cond
     ((memq element-type '(src-block example-block fixed-width))
      (kill-new (org-trim
                 (org-remove-indentation
                  (org-element-property :value element)))))
     (t (error "No element to copy")))))

(provide 'org-copy-for-external)

;;; org-copy-for-external.el ends here

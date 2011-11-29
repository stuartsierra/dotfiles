;;; hlinum.el --- Extension for linum.el to highlight current line number

;; Copyright (C) 2011  by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; Keywords: convenience, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extension for linum-mode to highlight current line number.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'hlinum)
;; And by using M-x linum-mode, you can see line numbers
;; with highlighting current line number.
;;
;; You can customize the color of highlighting current line by
;; changing `linum-highlight-face'.

;;; Code:

(require 'linum)

(defface linum-highlight-face
    '((t (:inherit default :foreground "black"
          :background "gray")))
  "Face for highlighting current line"
  :group 'linum)

(defun hlinum-find-if (predicate seq)
  (let (ret)
    (while (and (null ret) seq)
      (when (funcall predicate (car seq))
        (setq ret (car seq)))
      (setq seq (cdr seq)))
    ret))

(defun linum-color (face)
  "Highlight current line number by using face FACE."
  (save-excursion
    (let* ((pt (max (window-start)
                    (progn (move-beginning-of-line nil)
                           (point))))
           (ov (hlinum-find-if
                (lambda (e) (stringp (overlay-get e 'linum-str)))
                (overlays-in pt pt))))
      (when ov
        (let* ((str (overlay-get ov 'before-string))
               (lstr (overlay-get ov 'linum-str))
               (nov (move-overlay ov pt pt)))
          (add-text-properties 0 (string-width lstr)
                               `(face ,face) lstr)
          (add-text-properties 0 1 `(display ((margin left-margin)
                                              ,lstr)) str)
          (overlay-put nov 'before-string str)
          (overlay-put nov 'linum-str lstr))))))

(defun highlight-current-line ()
  (linum-color 'linum-highlight-face))
(defun unhighlight-current-line ()
  (linum-color 'linum))

(add-hook 'pre-command-hook 'unhighlight-current-line)
(defadvice linum-update-current (after linum-aft-cur activate)
  (highlight-current-line))
(defadvice linum-after-size (after linum-aft-size activate)
  (highlight-current-line))
(defadvice linum-after-scroll (after linum-aft-scl activate)
  (when (eq (current-buffer) (window-buffer))
    (highlight-current-line)))

(provide 'hlinum)
;;; hlinum.el ends here

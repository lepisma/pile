;;; pile-index.el --- Index parsing for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Index parsing for pile
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)
(require 'pile-utils)

(defun pile-index-parse-dir (dir)
  "Parse directory for org files recursively"
  (let ((items (f-entries dir))
        (output nil))
    (-each items
      (lambda (it)
        (cond ((and (f-file? it)
                    (s-ends-with? ".org" it)
                    (not (s-ends-with? "index.org" it))
                    (not (s-starts-with? ".#" (f-filename it))))
               (push it output))
              ((and (f-dir? it) (f-exists? (f-join it "index.org")))
               (progn
                 (push (f-join it "index.org") output)
                 (push (pile-index-parse-dir it) output))))))
    (reverse (-remove #'null output))))

(defun pile-index-format (index-tree &optional level)
  "Format index tree as org list"
  (let ((output "")
        (indent (s-join "" (-repeat (or level 0) "  "))))
    (-each index-tree
      (lambda (it)
        (cond ((stringp it)
               (let ((link-path (f-relative it default-directory))
                     (link-title (pile--file-title it)))
                 (setq output (s-append (format "%s- [[./%s][%s]]\n" indent link-path link-title) output))))
              ((consp it)
               (setq output (s-append (pile-index-format it (+ (or level 0) 1)) output))))))
    output))

(defun pile-index ()
  "Return string representation of the index tree"
  (pile-index-format (pile-index-parse-dir default-directory)))

(provide 'pile-index)

;;; pile-index.el ends here

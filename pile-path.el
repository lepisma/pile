;;; pile-path.el --- path utilities for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; path utilities for pile
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

(require 'f)
(require 's)
(require 'dash)
(require 'pile-utils)

(defun pile-path-rel-to-org (rel-path pj)
  "Return rel path of the .org file in that project."
  (let ((abs-path (f-join (oref pj :input-dir) rel-path)))
    (if (f-dir-p abs-path)
        (f-join rel-path "index.org")
      (format "%s.org" rel-path))))

(defun pile-path-parse (path)
  "Parse paths like wiki:this/that/ etc."
  (-let [(pname rest-path) (s-split-up-to ":" path 1)]
    (let ((splits (s-split-up-to "::" rest-path 1)))
      `((project . ,(pile-get-project pname))
        (rel-path . ,(car splits))
        (internal-path . ,(second splits))))))

(defun pile-path-abs (path)
  "Ignoring the internal link"
  (let* ((parse (pile-path-parse path))
         (pj (alist-get 'project parse)))
    (f-join (oref pj :input-dir) (pile-path-rel-to-org (alist-get 'rel-path parse) pj))))

(provide 'pile-path)

;;; pile-path.el ends here

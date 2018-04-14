;;; pile-link.el --- Org link for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Org link for pile
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
(require 'org)

(org-add-link-type "pile" #'pile-link-follow #'pile-link-export)

(defun pile-link--path (path)
  (let ((pile-path (f-join pile-source path)))
    (if (f-dir? pile-path)
        (f-join path "index.org")
      (format "%s.org" path))))

(defun pile-link-follow (path)
  "Open the path in a buffer"
  (find-file-existing (f-join pile-source (pile-link--path path))))

(defun pile-link-export (path desc backend)
  "Export fn for link"
  (if (eq backend 'html)
      (format "<a class=\"pile-link\" href=\"/%s/%s\">%s</a>" pile-base-url
              (f-swap-ext (pile-link--path path) "html") desc)))

(provide 'pile-link)

;;; pile-link.el ends here

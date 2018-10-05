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
(require 's)
(require 'org)
(require 'dash)
(require 'pile-utils)
(require 'pile-path)

(org-add-link-type "pile" #'pile-link-follow #'pile-link-export)

(defun pile-link-follow (path)
  "Open the path in a buffer"
  (let* ((parse (pile-path-parse path))
         (pj (alist-get 'project parse))
         (internal-suffix (--if-let (alist-get 'internal-path parse) (format "::%s" it) "")))
    (org-open-link-from-string
     (format "file:%s%s" (f-join (oref pj :input-dir)
                                 (pile-path-rel-to-org (alist-get 'rel-path parse) pj))
             internal-suffix))))

(defun pile-link-export (path desc backend)
  "Export fn for link"
  (if (eq backend 'html)
      (let* ((parse (pile-path-parse path))
             (pj (alist-get 'project parse))
             (internal-suffix (--if-let (alist-get 'internal-path parse) (format "#sec-%s" (pile--name-to-id it)) "")))
        (format "<a class=\"pile-link\" href=\"/%s/%s%s\">%s</a>" (oref pj :base-url)
                (f-swap-ext (pile-path-rel-to-org (alist-get 'rel-path parse) pj) "html")
                internal-suffix desc))))

(provide 'pile-link)

;;; pile-link.el ends here

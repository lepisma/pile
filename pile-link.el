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

(org-add-link-type "pile" #'pile-link-follow #'pile-link-export)

(defun pile-link--path (path root-dir)
  (let ((pile-path (f-join root-dir path)))
    (if (f-dir? pile-path)
        (f-join path "index.org")
      (format "%s.org" path))))

(defun pile-link-parse-path (path)
  (let* ((splits (s-split-up-to ":" path 1)))
    (list (second splits)
          (-find (lambda (pj) (string-equal (car splits) (oref pj :name))) pile-projects))))

(defun pile-link-follow (path)
  "Open the path in a buffer"
  (-let [(p-path pj) (pile-link-parse-path path)]
    (let ((root-dir (oref pj :input-dir)))
      (find-file-existing (f-join root-dir (pile-link--path p-path root-dir))))))

(defun pile-link-export (path desc backend)
  "Export fn for link"
  (-let [(p-path pj) (pile-link-parse-path path)]
    (if (eq backend 'html)
        (format "<a class=\"pile-link\" href=\"/%s/%s\">%s</a>" (oref pj :base-url)
                (f-swap-ext (pile-link--path p-path (oref pj :input-dir)) "html") desc))))

(provide 'pile-link)

;;; pile-link.el ends here

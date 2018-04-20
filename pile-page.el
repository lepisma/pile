;;; pile-page.el --- Page abstraction for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Page abstraction for pile
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

(require 'eieio)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)
(require 'cl-lib)

(defclass pile-page ()
  ((title :initarg :title
          :type string
          :documentation "Title of the page")
   (pile-path :initarg :pile-path
              :type string
              :documentation "Pile path as used in links. Relative to pile root")
   (org-path :initarg :org-path
             :type string
             :documentation "Path to the main org file relative to pile root"))
  "An org-file / directory mapping to one html output in the export")

(defun pile-page--full-path (path)
  "Return full absolute path"
  (f-join pile-source path))

(defun pile-page--file-title (org-file)
  "Get title for an org file. Path is relative to pile-source"
  (let ((lines (s-split "\n" (f-read-text (pile-page--full-path org-file)))))
    (second (s-split "TITLE: " (-find (-cut s-starts-with? "#+TITLE:" <>) lines)))))

(defun pile-page--file-exists? (path)
  "Tell if the pile path exists"
  (f-exists? (pile-page--full-path path)))

(cl-defmethod pile-page-valid? ((page pile-page))
  "Tell if a page is valid"
  (f-exists? (f-join pile-source (oref page :org-path))))

(defun pile-page-from-path (path)
  "Create a new pile page"
  (cl-multiple-value-bind (org-path pile-path)
      (cond ((s-ends-with? "index.org" path) (cl-values path (f-parent path)))
            ((s-ends-with? ".org" path) (cl-values path (substring path 0 (- (length path) 4))))
            ((pile-page--file-exists? (s-concat path ".org")) (cl-values (s-concat path ".org") path))
            (t (cl-values (f-join path "index.org") path)))
    (pile-page :title (pile-page--file-title org-path)
               :pile-path pile-path
               :org-path org-path)))

(cl-defmethod pile-page-parent ((page pile-page))
  "Return parent pile-page for given PAGE"
  (let ((pile-path (oref page :pile-path)))
    (if (find pile-path '("/" "")) nil
      (let ((parent-path (f-parent pile-path)))
        (pile-page-from-path (if (string= parent-path "./") "" parent-path))))))

(provide 'pile-page)

;;; pile-page.el ends here

;;; pile-bc.el --- Breadcrumbs for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Breadcrumbs for pile
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
(require 'pile-utils)
(require 's)

(defun pile-bc--parents (rel-path)
  "Break path into roots"
  (let* ((splits (reverse (s-split "/" rel-path)))
         (dir-page? (string-equal (car splits) "index"))
         (offset (if dir-page? 2 1))
         (parents (nthcdr offset splits)))
    (reverse
     (-map-indexed (lambda (index item)
                     (let ((item-path))
                       (cons item
                             (concat (s-repeat (+ offset index) "../") item "/index.html"))))
                   parents))))

(defun pile-bc--page-title (rel-path)
  "Return page title for the path given"
  (let ((splits (reverse (s-split "/" rel-path))))
    (if (string-equal (car splits) "index")
        (or (second splits) "home")
      (car splits))))

(defun pile-bc--linkify-parents (parents)
  "Return concatenated html for parents"
  (funcall 's-join " / " (-map (lambda (parent)
                                 (format "<a href='%s'>%s</a>" (cdr parent) (car parent)))
                               parents)))

(defun pile-bc--linkify-root (rel-path root-dir)
  "Return link for root file"
  (let* ((root-input-file "index.org")
         (full-path (f-join root-dir rel-path))
         (root-rel-path (f-relative (f-join root-dir root-input-file) full-path))
         (root-output-file (f-swap-ext root-rel-path "html")))
    (format "<a href='%s'>%s</a>" (substring-no-properties root-output-file 1) "≡ index")))

(defun pile-bc-generate-breadcrumbs (rel-path root-dir)
  "Generate html breadcrumbs"
  (let ((parents (pile-bc--parents rel-path)))
    (format "#+HTML:<div id='breadcrumbs'>%s / %s %s</div>"
            (pile-bc--linkify-root rel-path root-dir)
            (if (zerop (length parents)) "" (format "%s /" (pile-bc--linkify-parents parents)))
            (pile-bc--page-title rel-path))))

(defun pile-bc-added? ()
  "Tell if we already have breadcrumbs."
  (goto-char (point-min))
  (re-search-forward "^#\\+HTML:<div id='breadcrumbs'>" nil t))

(defun pile-bc-skip ()
  "Place point just after breadcrumb"
  (goto-char (point-min))
  (re-search-forward "^#\\+HTML:<div id='breadcrumbs'>" nil t)
  (end-of-line))

(defun pile-bc-add ()
  "Function to insert breadcrumbs in the current file."
  (unless (pile-bc-added?)
    (let* ((fname (buffer-file-name))
           (pj (pile-get-project-from-file fname))
           (rel-path (s-chop-suffix ".org" (f-relative fname (oref pj :input-dir)))))
      (pile--goto-top)
      (insert (pile-bc-generate-breadcrumbs rel-path (oref pj :input-dir))))))

(provide 'pile-bc)

;;; pile-bc.el ends here

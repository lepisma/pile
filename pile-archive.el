;;; pile-archive.el --- Archive listing for blog projects -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Archive listing for blog projects
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
(require 'dash)
(require 'pile-utils)
(require 'pile-tags)
(require 'pile-date)

(defun pile-archive-parse ()
  "Parse the list of pages from the current root directory"
  (let ((org-files (-map (lambda (pr)
                           (if (not (s-ends-with? ".org" pr))
                               (f-join pr "index.org")
                             pr))
                         (f-glob "*/*/*/*"))))
    (-map (lambda (fname)
            (let ((buffer (find-file-noselect fname))
                  (link (f-relative fname default-directory))
                  (title (pile--file-title fname)))
              (with-current-buffer buffer
                `((link . ,(concat "./" link))
                  (title . ,title)
                  (tags . ,(pile-tags-parse-buffer))
                  (date . ,(pile-date-parse-date)))))) org-files)))

(defun pile-archive-format (page-alist)
  (let ((tags (cdr (assoc 'tags page-alist))))
    (format "
#+HTML: <div class='archive-item'>
#+HTML: <div class='page-meta'>%s</div>
#+ATTR_HTML: :class archive-title
[[file:%s][%s]]
%s
#+HTML: </div>"
            (cdr (assoc 'date page-alist))
            (cdr (assoc 'link page-alist))
            (cdr (assoc 'title page-alist))
            (if tags (pile-tags-format-tags tags) ""))))

(defun pile-archive ()
  (let ((items (pile-archive-parse)))
    (s-join "\n" (-map #'pile-archive-format (-sort (lambda (a b)
                                                      (string-lessp (cdr (assoc 'date b))
                                                                    (cdr (assoc 'date a))))
                                                    items)))))

(provide 'pile-archive)

;;; pile-archive.el ends here
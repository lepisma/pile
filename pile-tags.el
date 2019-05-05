;;; pile-tags.el --- Tag management for blog posts -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Tag management for blog posts
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
(require 'pile-utils)

(defun pile-tags-parse-buffer ()
  "Return a list of tags from the current buffer."
  (goto-char (point-min))
  (when (re-search-forward "^#\\+TAGS:\\(.*\\)$" nil t)
    (-map #'s-trim (s-split "," (match-string-no-properties 1)))))

(defun pile-tags-format-tags (tags &optional archive-page)
  "Format tags and return html"
  (concat "#+BEGIN_EXPORT html
<div class='page-tags'>"
          (s-join " " (-map (lambda (tag) (format "<a href='%s#%s'>%s</a>"
                                             (or archive-page "") tag tag)) tags))
          "</div>
#+END_EXPORT"))

(defun pile-tags-add ()
  "Function to insert tag list in the exported file"
  (let* ((fname (buffer-file-name))
         (pj (pile-get-project-from-file fname)))
    (unless (string-equal fname (f-join (oref pj :input-dir) "index.org"))
      (let ((tags (pile-tags-parse-buffer)))
        (pile--goto-top)
        (insert (pile-tags-format-tags tags (f-relative (oref pj :input-dir) (f-parent fname))))))))

(provide 'pile-tags)

;;; pile-tags.el ends here

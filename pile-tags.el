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

(require 'org)
(require 'ox)
(require 'f)
(require 's)

(defun pile-tags-parse-buffer (buffer)
  "Return a list of tags from the buffer"
  (with-current-buffer buffer
    (goto-char 0)
    (if (search-forward "#+TAGS:" nil t)
        (let* ((text (buffer-substring-no-properties (point) (line-end-position))))
          (-map #'s-trim (s-split "," text))))))

(defun pile-tags-format-tags (tags)
  "Format tags and return html"
  (format "#+HTML: <div class='page-tags'>%s</div>" (s-join "" (-map (lambda (tag) (format "<a href='#'>%s</a>" tag)) tags))))

(defun pile--at-header? ()
  "Return if at a header or empty line"
  (let ((line-text (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (or (s-equals? line-text "")
        (and (s-starts-with? "#+" line-text)
             (not (s-starts-with? "#+begin" (downcase line-text)))))))

(defun pile--goto-top ()
  "Move point to the top of file just after the headers"
  (goto-char 0)
  (if (search-forward "#+SETUPFILE:" nil t)
      (progn
        (while (pile--at-header?) (next-line))
        (previous-line)
        (insert "\n"))
    (signal 'error (format "SETUPFILE line not found in %s." buffer-file-name))))

(defun pile-tags-hook (_)
  "Function to insert tag list in the exported file"
  (let ((tags (pile-tags-parse-buffer (current-buffer))))
    (unless (string-equal "sitemap.org" (f-filename (buffer-file-name)))
      (pile--goto-top)
      (insert (pile-tags-format-tags tags)))))

(provide 'pile-tags)

;;; pile-tags.el ends here

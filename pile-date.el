;;; pile-date.el --- Insert date in blog posts -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Insert date in blog posts
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

(defun pile-date-parse-date (&optional file-path)
  "Get date from the buffer path"
  (let* ((path (or file-path (buffer-file-name)))
         (date-path (if (string-equal "index.org" (f-filename path))
                        (f-parent (f-parent path))
                      (f-parent path))))
    (s-replace "/" "-" (substring date-path -10))))

(defun pile-date-added? ()
  (goto-char (point-min))
  (re-search-forward "^#\\+DATE:" nil t))

(defun pile-date-add ()
  "Function to insert date information in the exported file"
  (unless (pile-date-added?)
    (let* ((fname (buffer-file-name))
           (pj (pile-get-project-from-file fname)))
      (unless (string-equal fname (f-join (oref pj :input-dir) "index.org"))
        (goto-char 1)
        (search-forward "#+TITLE:")
        (goto-char (line-end-position))
        (insert "\n")
        (insert (format "#+DATE: <%s>" (pile-date-parse-date)))))))

(provide 'pile-date)

;;; pile-date.el ends here

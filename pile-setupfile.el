;;; pile-setupfile.el --- Auto insert setupfile in pile posts -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Auto insert setupfile in pile posts
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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'pile-utils)

(defun pile-setupfile-added? ()
  "Tell if a setupfile is already added.

This is neither case-sensitive, nor takes care of the possibility
of multiple setupfiles."
  (goto-char (point-min))
  (re-search-forward "^#\\+SETUPFILE:" nil t))

(defun pile-setupfile-add ()
  "Function to insert default setupfile for the project in the org
file."
  (unless (pile-setupfile-added?)
    (let* ((fname (buffer-file-name))
           (pj (pile-get-project-from-file fname))
           (setupfile (oref pj :setupfile)))
      (unless (string-equal setupfile "")
        (goto-char 1)
        (search-forward "#+TITLE:")
        (goto-char (line-end-position))
        (insert "\n")
        (insert "#+SETUPFILE: " setupfile)))))

(provide 'pile-setupfile)

;;; pile-setupfile.el ends here

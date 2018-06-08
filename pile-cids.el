;;; pile-cids.el --- Custom ids for headings -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Custom ids for headings
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

(require 's)

(defun pile-cids-present-p ()
  "Check if a cid is already present"
  (org-entry-get (point) "CUSTOM_ID"))

(defun pile-cids-add-all ()
  "Apply cid to all the headings in this buffer"
  (org-map-entries (lambda () (unless (pile-cids-present-p) (pile-cids-add)))))

(defun pile-cids-add-all-hook (_)
  (pile-cids-add-all))

(defun pile-cids-get-all ()
  "Return all cids from the current buffer"
  (org-map-entries (lambda () (org-entry-get (point) "CUSTOM_ID"))))

(defun pile-cids-outline-to-id (outline)
  "Convert an outline to id"
  (s-replace-all '((" " . "-")) (downcase (s-join "/" outline))))

(defun pile-cids-add ()
  "Create/update a cid at point"
  (let ((outline (append (org-get-outline-path) (list (substring-no-properties (org-get-heading))))))
    (org-set-property "CUSTOM_ID" (pile-cids-outline-to-id outline))))

(defun pile-cids-clear-html-hook (_ifile ofile)
  "Clear CUSTOM_ID field from the generated html file"
  (if (s-ends-with? ".html" ofile)
      (with-current-buffer (find-file-noselect ofile)
        (goto-char (point-min))
        (while (re-search-forward "<pre class=\"example\">\\(.*\n\\)*?custom_id:.*$" nil t)
          (delete-line)
          (delete-char 1))
        (goto-char (point-min))
        (while (re-search-forward "<pre class=\"example\">\n*</pre>" nil t)
          (replace-match ""))
        (save-buffer)
        (kill-buffer))))

(provide 'pile-cids)

;;; pile-cids.el ends here

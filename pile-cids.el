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
(require 'pile-utils)

(defun pile-cids-present-p ()
  "Check if a cid is already present"
  (org-entry-get (point) "CUSTOM_ID"))

(defun pile-cids-add-all ()
  "Apply cid to all the headings in this buffer"
  (let ((flat-cid (eq 'flat (alist-get 'cid (pile-read-options))))
        (cid-counter (make-hash-table :test 'equal)))
    (org-map-entries
     (lambda ()
       (unless (pile-cids-present-p)
         (pile-cids-add cid-counter flat-cid))))))

(defun pile-cids-outline-to-id (outline counter)
  "Convert an OUTLINE to id and update the COUNTER."
  (let ((id (format "sec-%s" (pile--name-to-id (s-join "/" outline)))))
    (puthash id (+ 1 (gethash id counter 0)) counter)
    (let ((count (gethash id counter)))
      (if (= 1 count) id (format "%s-%s" id count)))))

(defun pile-cids-add (counter &optional flat)
  "Create/update a cid at point"
  (let* ((heading (substring-no-properties (org-get-heading)))
         (outline (if flat (list heading) (append (org-get-outline-path) (list heading)))))
    (org-set-property "CUSTOM_ID" (pile-cids-outline-to-id outline counter))))

(defun pile-cids-clear-html ()
  "Clear CUSTOM_ID field from the generated html file (current buffer)."
  (goto-char (point-min))
  (fundamental-mode)
  (while (re-search-forward "\\(<pre class=\"example\">\n*custom_id:.*\n</pre>\\)\\|<pre class=\"example\">\\(.*\n\\)+?\\(custom_id:.*\n\\)</pre>" nil t)
    ;; Matches are mutually exhaustive
    (if (match-beginning 1)
        (replace-match "" nil nil nil 1)
      (replace-match "" nil nil nil 3))))

(provide 'pile-cids)

;;; pile-cids.el ends here

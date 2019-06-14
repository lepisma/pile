;;; pile-issue.el --- Issue system for writing -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;;; Commentary:

;; Issue system for writing
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
(require 'org)

(org-add-link-type "pile-issue" #'pile-issue-follow #'pile-issue-export)

(defun pile-issue--find-issue (issue-id)
  "Return headline with given issue-id."
  (let ((found-heading nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (string= issue-id (org-element-property :ISSUE-ID headline))
          (setq found-heading headline))))
    found-heading))

(defun pile-issue-follow (issue-id)
  "Open issue annotation in a side buffer."
  (let* ((issues-file (concat (f-swap-ext (buffer-file-name) "issues") ".org"))
         (_ (find-file-other-window issues-file))
         (found-heading (pile-issue--find-issue issue-id)))
    (when found-heading
      (goto-char (org-element-property :contents-begin found-heading)))))

(defun pile-issue-export (issue-id _desc backend)
  (when (eq backend 'html)
    (format "<span class=\"pile-issue-icon\">⚠ %s</span>" issue-id)))

(provide 'pile-issue)

;;; pile-issue.el ends here

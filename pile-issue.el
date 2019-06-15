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
(require 'ht)
(require 'mustache)
(require 'pile-utils)

(org-add-link-type "pile-issue" #'pile-issue-follow #'pile-issue-export)

(defcustom pile-issue-file-template "#+TITLE: {{ page-title }}
#+SETUPFILE: {{ setupfile }}
"
  "Template for a new issue file.")

(defun pile-issue--max-id ()
  "Return maximum issue id. If nothing, return 0. Assume current
buffer to be an issue buffer."
  (let ((issue-ids))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let ((cid (org-element-property :CUSTOM_ID headline)))
          (when (string-match "pile-issue-\\([[:digit:]]+\\)" cid)
            (push (string-to-number (match-string 1)) issue-ids)))))
    (if issue-ids (apply #'max issue-ids) 0)))

(defun pile-issue--find-issue (issue-id)
  "Return headline with given issue-id."
  (let ((found-heading nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (string= (format "pile-issue-%s" issue-id) (org-element-property :CUSTOM_ID headline))
          (setq found-heading headline))))
    found-heading))

(defun pile-issue--file ()
  "Return issue file path for current buffer."
  (f-swap-ext (buffer-file-name) "issues.org"))

(defun pile-issue--create-file ()
  "Create an issue file for current pile buffer."
  (let ((issue-file (pile-issue--file)))
    (if (f-exists? issue-file)
        (message "Issues file already present.")
      (let ((title (concat (pile--name-to-id (pile--file-title (buffer-file-name))) "/issues"))
            (setupfile (concat (f-relative (locate-dominating-file "." "assets") ".") "assets/export.setup")))
        (with-current-buffer (find-file-noselect issue-file)
          (insert (mustache-render pile-issue-file-template
                                   (ht ("page-title" title) ("setupfile" setupfile))))
          (save-buffer))))))

(defun pile-issue-create-issue (title)
  "Create a new issue, incrementing the maximum id present in the
file."
  (interactive "sIssue title: ")
  (let ((issue-file (pile-issue--file)))
    (when (not (f-exists? issue-file))
      (pile-issue--create-file))
    (with-current-buffer (find-file-noselect issue-file)
      (let ((issue-id (+ 1 (pile-issue--max-id))))
        (goto-char (point-max))
        (insert "* " title "\n")
        (org-set-property "CUSTOM_ID" (format "pile-issue-%s" issue-id))))))

(defun pile-issue-follow (issue-id)
  "Open issue annotation in a side buffer."
  (let* ((issus-file (f-swap-ext (buffer-file-name) "issues.org"))
         (_ (find-file-other-window issue-file))
         (found-heading (pile-issue--find-issue issue-id)))
    (when found-heading
      (goto-char (org-element-property :contents-begin found-heading)))))

(defun pile-issue-export (issue-id _desc backend)
  (when (eq backend 'html)
    (format "<span class=\"pile-issue-icon\">⚠ %s</span>" issue-id)))

(provide 'pile-issue)

;;; pile-issue.el ends here

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

(defcustom pile-issue-file-template "#+TITLE: ~:{{ page-title }}/issues~
#+SETUPFILE: {{ setupfile }}
"
  "Template for a new issue file.")

(defun pile-issue? (headline)
  "Tell if this headline is an issue."
  (let ((cid (org-element-property :CUSTOM_ID headline)))
    (and cid (string-match "pile-issue-\\([[:digit:]]+\\)" cid))))

(defun pile-issue--parse-issues ()
  "Parse issues (headlines) from current buffer"
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline) (when (pile-issue? headline) headline))))

(defun pile-issue--id (headline)
  "Tell issue id for a given headline element (org-element API)
 if the headline is an issue."
  (let ((cid (org-element-property :CUSTOM_ID headline)))
    (when (string-match "pile-issue-\\([[:digit:]]+\\)" cid)
      (string-to-number (match-string 1 cid)))))

(defun pile-issue--max-id ()
  "Return maximum issue id. If nothing, return 0. Assume current
buffer to be an issue buffer."
  (let ((issues (pile-issue--parse-issues)))
    (if issues (apply #'max (mapcar #'pile-issue--id issues)) 0)))

(defun pile-issue--find-issue (issue-id)
  "Return headline with given issue-id."
  (let ((issues (pile-issue--parse-issues)))
    (find issue-id issues :key #'pile-issue--id)))

(defun pile-issue--file? (&optional file-path)
  "Tell if given path or current buffer is an issue file."
  (s-ends-with? "issues.org" (or file-path (buffer-file-name))))

(defun pile-issue-file ()
  "Return issue file path for current buffer."
  (if (pile-issue--file?)
      (buffer-file-name)
    (f-swap-ext (buffer-file-name) "issues.org")))

(defun pile-main-file ()
  "Return main file path for current buffer."
  (if (pile-issue--file?)
      (s-replace-regexp "\\(\\.issues\\)\\.org" "" (buffer-file-name) nil nil 1)
    (buffer-file-name)))

(defun pile-issue--create-file ()
  "Create an issue file for current pile buffer."
  (let ((issue-file (pile-issue-file)))
    (if (f-exists? issue-file)
        (message "Issues file already present.")
      (let ((title (pile--name-to-id (pile--file-title (buffer-file-name))))
            (setupfile (concat (f-relative (locate-dominating-file "." "assets") ".") "assets/export.setup")))
        (with-current-buffer (find-file-noselect issue-file)
          (insert (mustache-render pile-issue-file-template
                                   (ht ("page-title" title) ("setupfile" setupfile))))
          (save-buffer))))))

(defun pile-issue-create-issue (title)
  "Create a new issue, incrementing the maximum id present in the
file."
  (interactive "sIssue title: ")
  (let ((issue-file (pile-issue-file)))
    (when (not (f-exists? issue-file))
      (pile-issue--create-file))
    (with-current-buffer (find-file-noselect issue-file)
      (let ((issue-id (+ 1 (pile-issue--max-id))))
        (goto-char (point-max))
        (insert "* " title "\n")
        (org-set-property "CUSTOM_ID" (format "pile-issue-%s" issue-id))))))

(defun pile-issue-follow (issue-id-str)
  "Open issue annotation in a side buffer."
  (let* ((issue-file (pile-issue-file))
         (_ (find-file-other-window issue-file))
         (found-heading (pile-issue--find-issue (string-to-number issue-id-str))))
    (if found-heading
        (goto-char (org-element-property :contents-begin found-heading))
      (message "No issue with given id: %s" issue-id-str))))

(defun pile-issue-count (&optional unresolved-only)
  "Return count of issues for the current page. We don't go down
in the tree recursively. If UNRESOLVED-ONLY is not nil return
count of unresolved issues. Return 0 as the nil case."
  (let ((issue-file (pile-issue-file)))
    (if (not (f-exists? issue-file))
        0
      (with-current-buffer (find-file-noselect issue-file)
        (let ((headlines (pile-issue--parse-issues)))
          (if unresolved-only
              (length (-remove (lambda (headline) (eq 'done (org-element-property :todo-type headline))) headlines))
            (length headlines)))))))

(defun pile-issue-format-crosslink ()
  "Format links for insertion in crosslinks section."
  (if (pile-issue--file?)
      (format "<a href=\"./%s\" class=\"btn small\">🖹 | main text</a>"
              (f-filename (f-swap-ext (pile-main-file) "html")))
    (when (f-exists? (pile-issue-file))
      (format "<a href=\"./%s\" class=\"btn small highlight\">%s / %s | open issues</a>"
              (f-filename (f-swap-ext (pile-issue-file) "html"))
              (pile-issue-count t) (pile-issue-count)))))

(defun pile-issue-export (issue-id-str desc backend)
  (when (eq backend 'html)
    ;; TODO: This is not final, there might be better way to export links
    (format "<a href=\"hello\" class=\"btn btn-small\"> 	🛈  %s</a>" issue-id-str desc)))

(provide 'pile-issue)

;;; pile-issue.el ends here

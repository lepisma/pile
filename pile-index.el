;;; pile-index.el --- Index parsing for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Index parsing for pile
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

(require 'dash)
(require 'f)
(require 's)
(require 'pile-utils)
(require 'pile-issue)

(defcustom pile-index-ignore-patterns
  '("\\.issues\\.org$")
  "File patterns to ignore from index")

(defun pile-index--valid-dir? (dir)
  "Tell if a directory is a valid page source. We don't look
further into this when it's not."
  ;; When a directory has no index.org it is invisible to us. I don't see this
  ;; being a problem from whatever pages I have built.
  (and (f-dir? dir)
       (f-exists? (f-join dir "index.org"))))

(defun pile-index--valid-sibling? (file-path)
  "Tell if the given org file is a valid sibling of its
directory."
  (let ((index-file (f-join (f-parent file-path) "index.org")))
    (not (or (string= file-path index-file)
             ;; Temporary files
             (s-starts-with? ".#" (f-filename file-path))
             (-any (-cut string-match <> file-path) pile-index-ignore-patterns)))))

(defun pile-index--sibling-files (root-dir)
  "Return paths to sibling org files for the directory. Note that
index.org is the main org file, every other org file, with some
filtering is a sibling and leaf node in the tree."
  (-filter #'pile-index--valid-sibling? (f-glob "*.org" root-dir)))

(defun pile-index--org-to-alist (file-path)
  "Create an alist for given org file without adding any
children."
  `((path . ,file-path)
    (title . ,(pile--file-title file-path))
    (children)))

(defun pile-index-parse (root-dir)
  "Parse and return a tree of files. Each file is an alist with
an absolute path, title and a list of children if present."
  (when (pile-index--valid-dir? root-dir)
    (let ((index-file (f-join root-dir "index.org"))
          (sibling-files (pile-index--sibling-files root-dir))
          (sub-dirs (f-entries root-dir #'pile-index--valid-dir?)))
      ;; TODO: Reduce repetition in alist creation code.
      `((path . ,index-file)
        (title . ,(pile--file-title index-file))
        (children . ,(append (mapcar #'pile-index--org-to-alist sibling-files)
                             (mapcar #'pile-index-parse sub-dirs)))))))

(defun pile-index--issue-path (abs-path root-dir)
  "Whether to show issue link for the page. If yes, then return
the relative path to issue page."
  (with-current-buffer (find-file-noselect abs-path)
    (when (< 0 (pile-issue-count t))
      (f-relative (pile-issue-file) root-dir))))

(defun pile-index--issue-page-link (rel-link)
  (format "@@html:<a href=\"./%s\" title=\"open issues page\">ⓘ </a>@@" rel-link))

(defun pile-index--format-node (node root-dir level)
  "Format a single entry."
  (let* ((indent (s-join "" (-repeat level "  ")))
         (abs-path (alist-get 'path node))
         (issue-path (pile-index--issue-path abs-path root-dir)))
    (format "%s- [[./%s][%s]]%s\n"
            indent
            (f-relative abs-path root-dir)
            (alist-get 'title node)
            (if issue-path (concat " | " (pile-index--issue-page-link (f-swap-ext issue-path "html"))) ""))))

(defun pile-index--format (index-tree root-dir &optional level)
  "Format index tree as org list"
  (let ((output ""))
    (-each (alist-get 'children index-tree)
      (lambda (node)
        (setq output (s-append (pile-index--format-node node root-dir (or level 0)) output))
        (setq output (s-append (pile-index--format node root-dir (+ 1 (or level 0))) output))))
    output))

(defun pile-index-format ()
  "Return string representation of the index tree. Call this in
the index pages of wiki."
  (let ((index-tree (pile-index-parse default-directory)))
    (pile-index--format index-tree default-directory)))

(provide 'pile-index)

;;; pile-index.el ends here

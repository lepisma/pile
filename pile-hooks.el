;;; pile-hooks.el --- A few common hooks for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; A few common hooks for pile
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
;; along with this program. If not, see <http://WWW.gnu.org/licenses/>.

;;; Code:

(require 'pile-bc)
(require 'pile-cids)
(require 'pile-date)
(require 'pile-dropcap)
(require 'pile-tags)
(require 'pile-utils)
(require 's)

(defun pile-hooks-pre-add-bc ()
  "Add breadcrumbs to the wiki files."
  (pile-when-type '(wiki)
    (pile-bc-add)))

(defun pile-hooks-pre-add-cids ()
  "Add cids in the org file for wiki and blog projects."
  (pile-when-type '(blog wiki)
    (pile-cids-add-all)))

(defun pile-hooks-pre-add-date ()
  "Add date to org file for blog projects."
  (pile-when-type '(blog)
    (pile-date-add)))

(defun pile-hooks-pre-add-dropcap ()
  "Add dropcaps to org files for blog projects."
  (pile-when-type '(blog)
    (pile-dropcap-add)))

(defun pile-hooks-pre-add-tags ()
  "Add list of formatted tags to the buffer."
  (pile-when-type '(blog)
    (pile-tags-add)))

(defun pile-hooks-post-generate-atom (ifile ofile)
  "Regenerate atom files for the current project."
  (let ((pj (pile-get-project-from-file ifile)))
    (when (and (member (oref pj :type) '(blog))
               (s-ends-with-p ".html" ofile))
      (pile-atom-regenerate-page pj))))

(defun pile-hooks-post-clear-cids (ifile ofile)
  "Remove CUSTOM_ID related divs from the generated html."
  (let ((pj (pile-get-project-from-file ifile)))
    (when (and (member (oref pj :type) '(blog wiki))
               (s-ends-with-p ".html" ofile))
      (with-current-buffer (find-file-noselect ofile)
        (pile-cids-clear-html)
        (if (buffer-modified-p) (save-buffer))
        (kill-buffer)))))

(defun pile-hooks-post-stringify-title (ifile ofile)
  "Make the title plain text in the generated html."
  (let ((pj (pile-get-project-from-file ifile)))
    (when (and (member (oref pj :type) '(blog wiki))
               (s-ends-with-p ".html" ofile))
      (with-current-buffer (find-file-noselect ofile)
        (when (re-search-forward "<title>\\(.*\\)</title>" nil t)
          (let ((old-title (match-string-no-properties 1)))
            (replace-match (s-replace-regexp "<.*?>" "" old-title) nil nil nil 1)))
        (if (buffer-modified-p) (save-buffer))
        (kill-buffer)))))

(defun pile-hooks-post-generate-archive (ifile ofile)
  "Regenerate the archive (index page) for the project."
  (let ((pj (pile-get-project-from-file ifile)))
    (when (and (member (oref pj :type) '(blog))
               (s-ends-with-p ".html" ofile))
      (pile-archive-regenerate-page pj))))

(provide 'pile-hooks)

;;; pile-hooks.el ends here

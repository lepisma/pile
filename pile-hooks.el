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

(require 'pile-atom)
(require 'pile-bc)
(require 'pile-cids)
(require 'pile-date)
(require 'pile-dropcap)
(require 'pile-index)
(require 'pile-setupfile)
(require 'pile-tags)
(require 'pile-utils)
(require 's)

(defun pile-hooks-pre-add-bc (_export-backend)
  "Add breadcrumbs to the wiki files."
  (pile-when-type '(wiki)
    (pile-bc-add)))

(defun pile-hooks-pre-add-crosslinks (_export-backend)
  "Add cross links for given page. This needs to be called after
bc hook."
  ;; TODO: Move this somewhere safe
  (pile-when-type '(wiki)
    (goto-char (point-min))
    (unless (re-search-forward "^#\\+HTML:<div id='crosslinks'>" nil t)
      (pile-bc-skip)
      (insert "\n")
      (let ((links))
        (-if-let (issue-link (pile-issue-format-crosslink))
            (push issue-link links))
        (insert (format "#+HTML:<div id='crosslinks'>%s</div>" (s-join " " links)))))))

(defun pile-hooks-pre-add-cids (_export-backend)
  "Add cids in the org file."
  (pile-when-type '(blog wiki plain)
    (pile-cids-add-all)))

(defun pile-hooks-pre-add-date (_export-backend)
  "Add date to org file for blog projects."
  (pile-when-type '(blog)
    (pile-date-add)))

(defun pile-hooks-pre-add-setupfile (_export-backend)
  "Add setupfile line to org file. This should be executed first."
  (pile-setupfile-add))

(defun pile-hooks-pre-add-dropcap (_export-backend)
  "Add dropcaps."
  (pile-when-type '(blog plain)
    (pile-dropcap-add)))

(defun pile-hooks-pre-add-tags (_export-backend)
  "Add list of formatted tags to the buffer."
  (pile-when-type '(blog)
    (pile-tags-add)))

(defun pile-hooks-post-generate-atom (ifile ofile)
  "Regenerate atom files for the current project."
  (let ((pj (pile-get-project-from-file ifile)))
    (pile-when-project-type pj '(blog)
      (when (s-ends-with-p ".html" ofile)
        (pile-atom-generate pj)))))

(defun pile-hooks-post-clear-cids (ifile ofile)
  "Remove CUSTOM_ID related divs from the generated html."
  (let ((pj (pile-get-project-from-file ifile)))
    (pile-when-project-type pj '(blog wiki plain)
      (when (s-ends-with-p ".html" ofile)
        (pile-temp-open ofile
          (pile-cids-clear-html)
          (if (buffer-modified-p) (save-buffer)))))))

(defun pile-hooks-post-stringify-title (ifile ofile)
  "Make the title plain text in the generated html."
  (let ((pj (pile-get-project-from-file ifile)))
    (pile-when-project-type pj '(blog wiki plain)
      (when (s-ends-with-p ".html" ofile)
        (pile-temp-open ofile
          (goto-char (point-min))
          (when (re-search-forward "<title>\\(.*\\)</title>" nil t)
            (let ((old-title (match-string-no-properties 1)))
              (replace-match (s-replace-regexp "<.*?>" "" old-title) nil nil nil 1)))
          (if (buffer-modified-p) (save-buffer)))))))

(defun pile-hooks-post-sync-static-files (ifile ofile)
  "Sync static files (non-org) from input directory to output
directory."
  ;; First we delete all items from output directory.
  (let ((pj (pile-get-project-from-file ifile)))
    (pile-when-project-type pj '(wiki)
      ;; TODO: Activate this for blog
      (dolist (entry (pile-get-static-items ofile))
        (f-delete entry t))
      ;; Then copy over items from input to output.
      (let ((output-dir (f-parent ofile)))
        (dolist (entry (pile-get-static-items ifile))
          (f-copy entry (file-name-as-directory output-dir)))))))

(defun pile-hooks-post-generate-archive (ifile ofile)
  "Regenerate the archive (index page) for the project."
  (let ((pj (pile-get-project-from-file ifile)))
    (pile-when-project-type pj '(blog)
      (when (and (s-ends-with-p ".html" ofile)
                 (not (pile-archive-page-p ifile)))
        (let ((index-file (f-join (oref pj :input-dir) "index.org")))
          (when (f-exists? index-file)
            (pile-temp-open index-file
              (pile-publish-current-file t))))))))

(defun pile-hooks-post-generate-index (ifile _ofile)
  "Refresh indices for wiki tree on export. We keep walking up
the hierarchy starting from ifile and regenerate all the pages involved."
  (let ((pj (pile-get-project-from-file ifile)))
    (pile-when-project-type pj '(wiki)
      (let ((parent-index-file (if (s-ends-with? "index.org" ifile)
                                   (f-join (f-parent (f-parent ifile)) "index.org")
                                 (f-join (f-parent ifile) "index.org"))))
        (when (f-exists? parent-index-file)
          (pile-temp-open parent-index-file
            (pile-publish-current-file t)))))))

(provide 'pile-hooks)

;;; pile-hooks.el ends here

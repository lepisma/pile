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

(require 'eieio)
(require 'pile-atom)
(require 'pile-bc)
(require 'pile-cids)
(require 'pile-date)
(require 'pile-dropcap)
(require 'pile-index)
(require 'pile-setupfile)
(require 'pile-tags)
(require 'pile-utils)
(require 'pile-watermark)
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

(defun pile-hooks-pre-add-draft-watermark (_export-backend)
  "Add DRAFT watermark in any page with draft tag set to true."
  (when (alist-get 'draft (pile-read-options))
    (pile-watermark-add "DRAFT")))

(defmacro pile-hooks-html-post-processing-factory (post-processing-fns)
  "Take all HTML buffer POST-PROCESSING-FNS and return a single hook that
runs them one by one."
  `(lambda (ifile ofile)
     (let ((pj (pile-get-project-from-file ifile)))
       (pile-when-project-type pj '(blog wiki plain)
         (when (s-ends-with-p ".html" ofile)
           (pile-temp-open ofile
             (mapc #'funcall ,post-processing-fns)
             (if (buffer-modified-p) (save-buffer))))))))

(defun pile-hooks-post-generate-atom (ifile ofile)
  "Regenerate atom files for the current project."
  (let ((pj (pile-get-project-from-file ifile)))
    (pile-when-project-type pj '(blog)
      (when (s-ends-with-p ".html" ofile)
        (pile-atom-generate pj)))))

(defun pile-hooks-post-sync-static-files (ifile ofile)
  "Sync static files (non-org) from input directory to output
directory."
  (when (s-ends-with-p ".html" ofile)
    (let ((pj (pile-get-project-from-file ifile)))
      (pile-when-project-type pj '(blog wiki)
        ;; In case this is the root page, we will not do anything since root
        ;; pages could have some other sources of static files.
        (unless (pile-archive-page-p ifile)
          ;; First we delete all items from output directory.
          (dolist (entry (pile-get-static-items ofile))
            (message "[pile] Going to delete: %s" entry)
            (f-delete entry t))
          ;; Then copy over items from input to output.
          (let ((output-dir (f-parent ofile)))
            (dolist (entry (pile-get-static-items ifile))
              (message "[pile] Going to copy: %s" entry)
              (f-copy entry (file-name-as-directory output-dir)))))))))

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

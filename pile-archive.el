;;; pile-archive.el --- Archive listing for blog projects -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Archive listing for blog projects
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
(require 'helm)
(require 'pile-blog)
(require 'pile-date)
(require 'pile-tags)
(require 'pile-utils)

(defun pile-archive-parse ()
  "Parse the list of pages from the current root directory.
TODO: This is a mess. Some times I am working with a fname arg,
      other times with (current?) buffer."
  (let ((org-files (-map (lambda (pr)
                           (if (not (s-ends-with? ".org" pr))
                               (f-join pr "index.org")
                             pr))
                         (f-glob "*/*/*/*"))))
    (-map (lambda (fname)
            (let ((link (f-relative fname default-directory))
                  (title (pile--file-title fname)))
              (with-temp-buffer
                (insert-file-contents-literally fname nil nil 1000)
                `((link . ,(concat "./" link))
                  (title . ,title)
                  (draft . ,(cdr (assoc "draft" (pile-read-options))))
                  (tags . ,(pile-tags-parse-buffer))
                  (date . ,(pile-date-parse-date fname)))))) org-files)))

(defun pile-archive-draft-p (item)
  (alist-get 'draft item))

(defun pile-archive-unique-tags (items)
  "Parse unique tags from the page items"
  (-sort #'string-lessp (-uniq (-mapcat (lambda (item) (cdr (assoc 'tags item))) items))))

(defun pile-archive-tag-cloud ()
  "Return a tag cloud"
  (let ((items (-remove #'pile-archive-draft-p (pile-archive-parse))))
    (pile-tags-format-tags (pile-archive-unique-tags items))))

(defun pile-archive-format (page-alist)
  (let ((tags (cdr (assoc 'tags page-alist))))
    (format "
#+HTML: <div class='archive-item'>
#+HTML: <div class='page-meta'>%s</div>
#+ATTR_HTML: :class archive-title
[[file:%s][%s]]
%s
#+HTML: </div>"
            (cdr (assoc 'date page-alist))
            (cdr (assoc 'link page-alist))
            (cdr (assoc 'title page-alist))
            (if tags (pile-tags-format-tags tags) ""))))

(defun pile-archive ()
  (let ((items (-remove #'pile-archive-draft-p (pile-archive-parse))))
    (s-join "\n" (-map #'pile-archive-format (-sort (lambda (a b)
                                                      (string-lessp (cdr (assoc 'date b))
                                                                    (cdr (assoc 'date a))))
                                                    items)))))

(defun pile-archive-regenerate-page (pj)
  "Regenerate the index.org page for the blog type project."
  (let ((index-file (f-join (oref pj :input-dir) "index.org")))
    (if (f-exists-p index-file)
        (with-current-buffer (find-file-noselect index-file)
          (pile-publish-current-file t)))))

(defun pile-archive-page-p (file-path)
  "Tell if the page is 'the' archive page for its project."
  (let ((pj (pile-get-project-from-file file-path)))
    (string= file-path (f-join (oref pj :input-dir) "index.org"))))

;;;###autoload
(defun pile-archive-regenerate ()
  (interactive)
  (helm :sources (helm-build-sync-source "Pile blog projects"
                   :candidates (mapcar (lambda (pj) (cons (oref pj :name) pj))
                                       (-filter #'pile-blog-valid-project-p pile-projects))
                   :action #'pile-archive-regenerate-page)))


(provide 'pile-archive)

;;; pile-archive.el ends here

;;; pile-atom.el --- Atom feed generator for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Atom feed generator for pile
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
(require 'helm)
(require 'ht)
(require 'mustache)
(require 'pile-blog)
(require 'pile-path)
(require 's)

(defvar pile-atom-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>{{ root-title }}</title>
  <link href=\"{{ root-url }}\"/>
  <updated>{{ root-date }}</updated>
  <author><name>{{ root-author }}</name></author>

  {{#entry}}
  <entry>
    <title>{{ title }}</title>
    <link href=\"{{ link }}\"/>
    <updated>{{ date }}</updated>
  </entry>
  {{/entry}}
</feed>"
  "Template for atom.xml file")

(defun pile-atom-parse-item (pj item)
  "Create an ht for the ITEM generated from pile-archive."
  (let* ((link (alist-get 'link item))
         (new-link (concat (file-name-as-directory (oref pj :root-url))
                           (file-name-as-directory (oref pj :base-url))
                           (s-chop-prefix "./" (s-replace-regexp "\\.org$" ".html" link)))))
    (setf (alist-get 'link item) new-link)
    (let ((item (mapcar (lambda (kv) (cons (symbol-name (car kv)) (cdr kv))) item)))
      (ht<-alist item))))

(defun pile-atom-generate (pj)
  "Generate string representation of the feed for the project PJ."
  (when (eq (oref pj :type) 'blog)
    (let* ((default-directory (oref pj :input-dir))
           (items (sort (pile-archive-parse) (lambda (a b) (string-greaterp (alist-get 'date a) (alist-get 'date b))))))
      (mustache-render pile-atom-template
                       (ht ("root-author" (user-full-name))
                           ("root-title" (oref pj :name))
                           ("root-url" (concat (file-name-as-directory (oref pj :root-url))
                                               (oref pj :base-url)))
                           ("root-date" (format-time-string "%Y-%m-%d"))
                           ("entry" (mapcar (-cut pile-atom-parse-item pj <>) items)))))))

(defun pile-atom-regenerate-page (pj)
  (let ((text (pile-atom-generate pj))
        (feed-file (f-join (oref pj :input-dir) "atom.xml")))
    (if text
        (f-write-text text 'utf-8 feed-file)
      (message "Feed not generated."))))

(defun pile-atom-regenerate ()
  (interactive)
  (helm :sources (helm-build-sync-source "Pile blog projects"
                   :candidates (mapcar (lambda (pj) (cons (oref pj :name) pj))
                                       (cl-remove-if-not #'pile-blog-valid-project-p pile-projects))
                   :action #'pile-atom-regenerate-page)))

(provide 'pile-atom)

;;; pile-atom.el ends here

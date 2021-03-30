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

(require 'dash)
(require 'f)
(require 'helm)
(require 'ht)
(require 'mustache)
(require 'pile-base)
(require 'pile-archive)
(require 'pile-path)
(require 's)

(defvar pile-atom-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>{{ root-title }}</title>
  <link href=\"{{ root-url }}\"/>
  <link href=\"{{ root-url }}/atom.xml\" rel=\"self\"/>
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

(cl-defmethod pile-atom-parse-archive-item ((pj pile-project-blog) archive-item)
  "Create an ht for the ITEM generated from pile-archive."
  (let* ((link (alist-get 'link archive-item))
         (base-url-text (if (string-equal (oref pj :base-url) "") "" (file-name-as-directory (oref pj :base-url))))
         (new-link (concat (file-name-as-directory (oref pj :root-url))
                           base-url-text
                           (s-chop-prefix "./" (s-replace-regexp "\\.org$" ".html" link)))))
    (setf (alist-get 'link archive-item) new-link)
    (let ((archive-item (mapcar (lambda (kv) (cons (symbol-name (car kv)) (cdr kv))) archive-item)))
      (ht<-alist archive-item))))

(cl-defmethod pile-atom-format ((pj pile-project-blog))
  "Generate string representation of the feed for the project PJ."
  (let* ((default-directory (oref pj :input-dir))
         (items (->> (pile-archive-parse) (-remove #'pile-archive-draft-p) (-sort #'pile-archive-comparator))))
    (mustache-render pile-atom-template
                     (ht ("root-author" (user-full-name))
                         ("root-title" (oref pj :name))
                         ("root-url" (concat (file-name-as-directory (oref pj :root-url))
                                             (oref pj :base-url)))
                         ("root-date" (format-time-string "%FT%T%z"))
                         ("entry" (mapcar (lambda (it) (pile-atom-parse-archive-item pj it)) items))))))

(cl-defmethod pile-atom-generate ((pj pile-project-blog))
  "Regenerate atom.xml file for project PJ. Also copy the file in deploy
directory."
  (let ((text (pile-atom-format pj))
        (dirs (list (oref pj :input-dir) (oref pj :output-dir))))
    (when text
      (dolist (d dirs)
        (f-write-text text 'utf-8 (f-join d "atom.xml"))))))

(provide 'pile-atom)

;;; pile-atom.el ends here

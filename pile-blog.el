;;; pile-blog.el --- Utilities for blog posts -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Utilities for blog posts
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

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'helm)
(require 'pile-base)
(require 'pile-utils)
(require 'pile-archive)
(require 's)


(cl-defmethod pile-blog--create-post ((pj pile-project-blog) name)
  "Create a new post using current date."
  (cl-destructuring-bind (_ _ _ day month year _ _ _) (decode-time)
    (let* ((post-dir (f-join (oref pj :input-dir)
                             (number-to-string year)
                             (format "%02d" month)
                             (format "%02d" day)
                             (pile--name-to-id name)))
           (index-file (f-join post-dir "index.org")))
      (make-directory post-dir t)
      index-file)))

(cl-defmethod pile-blog-valid? ((pj pile-project-blog))
  "Tell if the project is a valid blog"
  (f-exists-p (f-join (oref pj :input-dir) "index.org")))

(defun pile-blog--choose-project (&optional helm-buffer-name)
  "Return a blog project using helm selector."
  (helm :sources (helm-build-sync-source "Pile blog projects"
                   :candidates (-map (lambda (pj) (cons (oref pj :name) pj))
                                     (-filter (lambda (pj) (and (pile-project-blog-p pj) (pile-blog-valid? pj))) pile-projects)))
        :buffer (or helm-buffer-name "*helm pile blog*")))

;;;###autoload
(defun pile-blog-new-post ()
  "Create a new post based on current date."
  (interactive)
  (let ((pj (pile-blog--choose-project "*helm pile blog new post*")))
    (find-file (pile-blog--create-post pj (read-string "Post name: ")))))

;;;###autoload
(defun pile-blog ()
  "Perform various acts on pile-blog posts."
  (interactive)
  (let* ((pj (pile-blog--choose-project))
         (default-directory (oref pj :input-dir))
         (items (->> (pile-archive-parse) (-sort #'pile-archive-comparator))))
    (helm :sources (helm-build-sync-source "Posts"
                     :candidates (-map (lambda (post) (cons (alist-get 'title post) post)) items)
                     :action `(("Open post" . (lambda (post) (pile-blog-open-post ,pj post)))
                               ("Insert link" . (lambda (post) (pile-blog-insert-link ,pj post)))
                               ("Delete post" . (lambda (post) (pile-blog-delete-post ,pj post)))
                               ("Redate post" . (lambda (post) (pile-blog-redate-post ,pj post)))))
          :buffer "*helm pile posts*")))

(defun pile-blog-open-post (pj post)
  "Open POST for editing."
  (find-file (f-join (oref pj :input-dir) (alist-get 'link post))))

(defun pile-blog-insert-link (pj post)
  "Insert Org Mode style pile link for given POST."
  (let ((link-name (read-from-minibuffer "Link name: " (alist-get 'title post))))
    (insert (format "[[pile:%s:%s][%s]]"
                    (oref pj :name) (alist-get 'link post) link-name))))

(defun pile-blog-swap-date-path (old-path new-date-string)
  "`new-date-string' is in YYYY-MM-DD format."
  (cl-destructuring-bind (year month date) (s-split "-" new-date-string)
    (when (string-match "\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)"  old-path)
      (-reduce-from (lambda (string repl-args)
                      (replace-match (car repl-args) nil nil string (cdr repl-args)))
                    old-path
                    `((,year . 1) (,month . 2) (,date . 3))))))

(defun pile-blog-redate-post (pj post)
  "Change the date of current post. This involves moving the item
from its current place both in source and deployment path. Also
cleanup of old-paths remnants."
  (let* ((new-date-string (org-read-date nil nil nil "New Date:"))
         (movable (pile-archive-movable post))
         (input-path (f-join (oref pj :input-dir) movable))
         (publish-path (f-join (oref pj :output-dir) movable)))
    (copy-directory input-path (pile-blog-swap-date-path input-path new-date-string) nil t t)
    (copy-directory publish-path (pile-blog-swap-date-path publish-path new-date-string) nil t t)
    (pile-blog-delete-post pj post)))

(defun pile-blog-delete-post (pj post)
  "Delete a post and its related files. also delete the deployed
page.

You will also need to regenerate feeds if you have the hooks
setup. We are not doing this here because it's a publish type
hook (maybe we need a structure change hook?) and also because
the effect won't be missed since any other publish will update
the feeds."
  (let* ((movable (pile-archive-movable post))
         (input-path (f-join (oref pj :input-dir) movable))
         (publish-path (f-join (oref pj :output-dir) movable)))
    (f-delete input-path t)
    (pile-clean-up-parents input-path)
    (when (f-exists? publish-path)
      (f-delete publish-path t)
      (pile-clean-up-parents publish-path))))

(provide 'pile-blog)

;;; pile-blog.el ends here

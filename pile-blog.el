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

(cl-defmethod pile-blog--choose-post ((pj pile-project-blog) &optional helm-buffer-name)
  "Return a post from the blog using a helm selector."
  (let* ((default-directory (oref pj :input-dir))
         (items (->> (pile-archive-parse) (-sort #'pile-archive-comparator))))
    (helm :sources (helm-build-sync-source "Posts"
                     :candidates (-map (lambda (post) (cons (alist-get 'title post) post)) items))
          :buffer (or helm-buffer-name "*helm pile posts*"))))

;;;###autoload
(defun pile-blog-new-post ()
  "Create a new post based on current date."
  (interactive)
  (let ((pj (pile-blog--choose-project "*helm pile blog new post*")))
    (find-file (pile-blog--create-post pj (read-string "Post name: ")))))

(defun pile-blog-refile-post (post)
  "Move this post to a new blog"
  (user-error "Not implemented"))

(defun pile-blog-redate-post (post)
  "Change the date of current post"
  (user-error "Not implemented"))

(defun pile-blog-delete-post (post &optional delete-deployed)
  "Delete a post and its related files, optionally also delete the deployed page."
  (user-error "Not implemented"))

(provide 'pile-blog)

;;; pile-blog.el ends here

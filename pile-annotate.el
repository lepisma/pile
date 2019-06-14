;;; pile-annotate.el --- Annotation system for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;;; Commentary:

;; Annotation system for pile
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

(org-add-link-type "pile-annotate" #'pile-annotate-follow #'pile-annotate-export)

(defun pile-annotate--find-annotation (annotation-id)
  "Return headline with given annotation-id."
  (let ((found-heading nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (string= annotation-id (org-element-property :ANNOTATION-ID headline))
          (setq found-heading headline))))
    found-heading))

(defun pile-annotate-follow (annotation-id)
  "Open annotation in a side buffer."
  (let* ((annot-file (concat (f-swap-ext (buffer-file-name) "annotations") ".org"))
         (_ (find-file-other-window annot-file))
         (found-heading (pile-annotate--find-annotation annotation-id)))
    (when found-heading
      (goto-char (org-element-property :contents-begin found-heading)))))

(defun pile-annotate-export (annotation-id desc backend)
  "Export list of items in the playlist"
  (when (eq backend 'html)
    (let* ((annot-file (concat (f-swap-ext (buffer-file-name) "annotations") ".org"))
           (_ (find-file-other-window annot-file))
           (found-heading (pile-annotate--find-annotation annotation-id))
           (org-export-show-temporary-export-buffer nil))
      (when found-heading
        (error "NI")))))


(provide 'pile-annotate)

;;; pile-annotate.el ends here

;;; pile-path.el --- path utilities for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; path utilities for pile
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
(require 's)
(require 'dash)
(require 'pile-utils)

;; Path types
;; ----------
;; abs-path : Absolute system path for the file.
;;            Example /home/lepisma/something.type
;; rel-path : Relative path of the file. Needs a project to get the root-dir.
;;            For example 2018/02/02/main.org
;; pile-path: Pile formatted path <project-name>:<rel-path>
;;            For example wiki:emacs/til

(defun pile-path-abs-project (abs-path)
  "Return project for the abs-path."
  (-find (lambda (pj) (f-ancestor-of? (oref pj :input-dir) abs-path)) pile-projects))

(defun pile-path-abs-to-pile (abs-path)
  "Return pile-path from the abs-path."
  (let ((pj (pile-path-abs-project abs-path)))
    (when pj
      (format "%s:%s" (oref pj :name) (f-relative abs-path (oref pj :input-dir))))))

(defun pile-path-abs-to-rel (abs-path)
  "Return rel path from the abs-path."
  (let ((pj (pile-path-abs-project abs-path)))
    (when pj
      (f-relative abs-path (oref pj :input-dir)))))

(defun pile-path-rel-to-org (rel-path pj)
  "Return rel path of the .org file in that project. Also works
for static files if found."
  (let ((abs-path (f-join (oref pj :input-dir) rel-path)))
    (cond ((f-dir-p abs-path) (f-join rel-path "index.org"))
          ((f-exists-p abs-path) rel-path)
          (t (format "%s.org" rel-path)))))

(defun pile-path-parse (pile-path)
  "Parse paths like wiki:this/that/ etc."
  (-let [(pname rest-path) (s-split-up-to ":" pile-path 1)]
    (let ((splits (s-split-up-to "::" rest-path 1)))
      `((project . ,(pile-get-project pname))
        (rel-path . ,(car splits))
        (internal-path . ,(second splits))))))

(defun pile-path-abs (pile-path)
  "Ignoring the internal link"
  (let* ((parse (pile-path-parse pile-path))
         (pj (alist-get 'project parse)))
    (f-join (oref pj :input-dir) (pile-path-rel-to-org (alist-get 'rel-path parse) pj))))

(provide 'pile-path)

;;; pile-path.el ends here

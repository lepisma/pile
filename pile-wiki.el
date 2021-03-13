;;; pile-wiki.el --- Utilities for wiki -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Utilities for wiki
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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'pile-base)
(require 'pile-archive)

(defun pile-wiki--choose-project (&optional helm-buffer-name)
  (let ((wiki-projects (-filter #'pile-project-wiki-p pile-projects)))
    (if (< (length wiki-projects) 2)
        (car wiki-projects)
      (helm :sources (helm-build-sync-source "Pile wiki projects"
                       :candidates (mapcar (lambda (pj) (cons (oref pj :name) pj)) wiki-projects))
            :buffer (or helm-buffer-name "*helm pile wiki*")))))

;;;###autoload
(defun pile-wiki ()
  "Perform various acts on pile-wiki pages."
  (interactive)
  (let* ((pj (pile-wiki--choose-project))
         (default-directory (oref pj :input-dir)))
    (error "Not implemented")))

(provide 'pile-wiki)

;;; pile-wiki.el ends here

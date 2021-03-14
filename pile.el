;;; pile.el --- Pile management

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "25") (dash "2.13.0") (dash-functional "2.13.0") (f "0.20.0") (s "1.12.0") (mustache "0.24") (ht "2.2"))
;; URL: https://github.com/lepisma/pile

;;; Commentary:

;; Org pile management
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
(require 'dash-functional)
(require 'f)
(require 'pile-base)
(require 'pile-utils)
(require 'pile-archive)
(require 'pile-bc)
(require 'pile-blog)
(require 'pile-cids)
(require 'pile-tags)
(require 'pile-date)
(require 'pile-dropcap)
(require 'pile-index)
(require 'pile-issue)
(require 'pile-path)
(require 'pile-link)
(require 'pile-serve)
(require 'pile-ref)
(require 'pile-atom)
(require 'pile-hooks)
(require 'pile-wiki)
(require 'org)
(require 'ox-html)
(require 'ox-publish)
(require 's)
(require 'helm)

(defgroup pile nil
  "Pile wiki"
  :group 'org)

(defcustom pile-projects nil
  "List of managed pile projects"
  :group 'pile)

(defcustom pile-pre-publish-hook nil
  "Hook for pre publish. Functions take no arguments and run in the
to-be-published buffer."
  :type 'hook
  :group 'pile)

(defcustom pile-post-publish-hook nil
  "Hook for post publish. Functions take the following arguments:
1. Input file path
2. Output file path
These functions are directly appended to org-publish-after-publishing-hook."
  :type 'hook
  :group 'pile)

(cl-defmethod pile-project-publish ((pj pile-project) &optional arg)
  "Publish the project"
  (save-excursion
    (with-pile-hooks (org-publish-project (format "pile-%s" (oref pj :name)) arg))))

;;;###autoload
(defun pile-publish-current-file (arg)
  "Publish only the current file"
  (interactive "P")
  (save-excursion
    (with-pile-hooks (org-publish-current-file arg))))

;;;###autoload
(defun pile-publish (arg)
  (interactive "P")
  (helm :sources (helm-build-sync-source "Pile projects"
                   :candidates (mapcar (lambda (pj) (cons (oref pj :name) pj)) pile-projects)
                   :action (lambda (pj) (pile-project-publish pj arg)))
        :buffer "*helm pile publish*"))

(defun pile-export-setup ()
  "A few custom org-export settings"
  (setq org-export-with-tags 'not-in-toc))

;;;###autoload
(defun pile-setup ()
  "Setup for pile"
  (let ((project-configs (mapcar #'pile-project-config pile-projects)))
    (setq org-publish-project-alist (apply #'append org-publish-project-alist project-configs)))
  (pile-export-setup)
  (pile-ref-setup))

(provide 'pile)

;;; pile.el ends here

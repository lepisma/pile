;;; pile.el --- Pile management

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.2.1
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
(require 'pile-utils)
(require 'pile-archive)
(require 'pile-bc)
(require 'pile-blog)
(require 'pile-cids)
(require 'pile-tags)
(require 'pile-date)
(require 'pile-dropcap)
(require 'pile-index)
(require 'pile-path)
(require 'pile-link)
(require 'pile-serve)
(require 'pile-ref)
(require 'pile-atom)
(require 'pile-hooks)
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

(defclass pile-project ()
  ((name :initarg :name
         :type string
         :documentation "Name for the project")
   (root-url :initarg :root-url
             :type string
             :documentation "Url for the deployed site")
   (base-url :initarg :base-url
             :type string
             :documentation "Url with respect to / at the host")
   (input-dir :initarg :input-dir
              :type string
              :documentation "Root input directory for project")
   (output-dir :initarg :output-dir
               :type string
               :documentation "Output directory for the project")
   (type :initarg :type
         :type symbol
         :documentation "Type of the project, a wiki or a blog")
   (postamble :initarg :postamble
              :type string
              :documentation "Postamble for the pages")
   (preamble :initarg :preamble
             :type string
             :documentation "Preamble for the pages")))

(cl-defmethod pile-project-static-config ((pj pile-project))
  "Get org-publish static config for the project"
  `(,(format "pile-%s-static" (oref pj :name))
    :base-directory ,(oref pj :input-dir)
    :base-extension ".*"
    :exclude ".*\.org$\\|.*export\.setup$\\|.*auto/.*\.el$\\|.*\.tex$\\|.*\.bib$"
    :recursive t
    :publishing-directory ,(oref pj :output-dir)
    :publishing-function org-publish-attachment))

(cl-defmethod pile-project-pages-config ((pj pile-project))
  "Get org-publish config for pages"
  (let ((type (oref pj :type)))
    `(,(format "pile-%s-pages" (oref pj :name))
      :auto-sitemap nil
      :base-directory ,(oref pj :input-dir)
      :recursive t
      :publishing-directory ,(oref pj :output-dir)
      :publishing-function org-html-publish-to-html
      :htmlized-source nil
      :html-doctype "html5"
      :html-checkbox-type unicode
      :html-html5-fancy t
      :html-postamble ,(oref pj :postamble)
      :html-preamble ,(oref pj :preamble))))

(cl-defmethod pile-project-config ((pj pile-project))
  "Pile project config for org-publish"
  (let ((name (oref pj :name)))
    (if (eq (oref pj :type) 'static)
        (list (pile-project-static-config pj)
              `(,(format "pile-%s" name)
                :components (,(format "pile-%s-static" name))))
      (list (pile-project-static-config pj)
            (pile-project-pages-config pj)
            `(,(format "pile-%s" name)
              :components (,(format "pile-%s-pages" name)
                           ,(format "pile-%s-static" name)))))))

(cl-defmethod pile-project-publish ((pj pile-project) &optional arg)
  "Publish the project"
  (save-excursion
    (with-pile-hooks (org-publish-project (format "pile-%s" (oref pj :name)) arg))))

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

;;;###autoload
(defun pile-setup ()
  "Setup for pile"
  (let ((project-configs (mapcar #'pile-project-config pile-projects)))
    (setq org-publish-project-alist (apply #'append org-publish-project-alist project-configs)))
  (pile-ref-setup))

(provide 'pile)

;;; pile.el ends here

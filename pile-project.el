;;; pile-project.el --- Project abstraction for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Project abstraction for pile
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

(defcustom pile-projects nil
  "Pile projects"
  :group 'pile)

(defclass pile-project ()
  ((name :initarg :name
         :type string
         :documentation "Name for the project")
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
    :exclude ".*\.org\\|.*export\.setup\\|.*auto/.*\.el\\|.*\.tex\\|.*\.bib"
    :recursive t
    :publishing-directory ,(oref pj :output-dir)
    :publishing-function org-publish-attachment))

(cl-defmethod pile-project-pages-config ((pj pile-project))
  "Get org-publish config for pages"
  `(,(format "pile-%s-pages" (oref pj :name))
    :auto-sitemap t
    :sitemap-filename "sitemap.org"
    :sitemap-title "Sitemap"
    ;; This will become conditional depending on what type of project this is
    :sitemap-format-entry pile-sitemap-entry
    :sitemap-function pile-sitemap
    :base-directory ,(oref pj :input-dir)
    :recursive t
    :publishing-directory ,(oref pj :output-dir)
    :publishing-function org-html-publish-to-html
    :htmlized-source nil
    :html-checkbox-type unicode
    :html-html5-fancy t
    :html-postamble ,(oref pj :postamble)
    :html-preamble ,(oref pj :preamble)))

(cl-defmethod pile-project-config ((pj pile-project))
  "Pile project config for org-publish"
  (let ((name (oref pj :name)))
    (list (pile-project-static-config pj)
          (pile-project-pages-config pj)
          `(,(format "pile-%s" name)
            :components (,(format "pile-%s-pages" name)
                         ,(format "pile-%s-static" name))))))

(provide 'pile-project)

;;; pile-project.el ends here

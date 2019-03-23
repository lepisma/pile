;;; pile-ref.el --- Functions for working with references -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Functions for working with references
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
(require 'org-ref)
(require 'ox-html)
(require 'pile-utils)

(defun pile-ref-update-bib ()
  (interactive)
  "Update the bib file for the current buffer."
  (if (pile-get-project-from-file (buffer-file-name))
      (let ((bib-file (f-join default-directory "references.bib")))
        (delete-file bib-file)
        (org-ref-extract-bibtex-to-file bib-file))
    (message "Buffer not part of any pile project")))

(defun pile-ref-setup ()
  (setq org-html-htmlize-output-type 'css
        org-ref-bibliography-entry-format
        '(("article" . "%a. %y. \"%t.\" <i>%j</i>, %v(%n), %p. <a class=\"bib-link\" href=\"%U\">link</a>. <a class=\"bib-link\" href=\"http://dx.doi.org/%D\">doi</a>.")
          ("book" . "%a. %y. <i>%t</i>. %u.")
          ("techreport" . "%a. %y. \"%t\", %i, %u.")
          ("proceedings" . "%e. %y. \"%t\" in %S, %u.")
          ("inproceedings" . "%a. %y. \"%t\", %p, in %b, edited by %e, %u"))))

(provide 'pile-ref)

;;; pile-ref.el ends here

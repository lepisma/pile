;;; pile-utils.el --- Some utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Some utilities
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

(defun pile--at-header? ()
  "Return if at a header or empty line"
  (let ((line-text (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (or (s-equals? line-text "")
        (and (s-starts-with? "#+" line-text)
             (not (s-starts-with? "#+begin" (downcase line-text)))))))

(defun pile--goto-top ()
  "Move point to the top of file just after the headers"
  (goto-char 0)
  (if (search-forward "#+SETUPFILE:" nil t)
      (progn
        (while (pile--at-header?) (next-line))
        (previous-line)
        (insert "\n"))
    (signal 'error (format "SETUPFILE line not found in %s." buffer-file-name))))

;;;###autoload
(defun pile-clear-cache ()
  "Clear org-publish-cache"
  (interactive)
  (setq org-publish-cache nil)
  (let ((cache-root (f-full "~/.emacs.d/.cache/.org-timestamps/")))
    (->> '("pile-pages.cache" "pile-static.cache")
       (-map (-cut f-join cache-root <>))
       (-filter #'f-exists?)
       (-map #'f-delete))))

(defmacro with-pile-hooks (hooks &rest body)
  "Run body with pile related export hooks set"
  `(condition-case err
       (progn
         (-each ,hooks (lambda (hook) (add-hook 'org-export-before-parsing-hook hook)))
         ,@body
         (-each ,hooks (lambda (hook) (remove-hook 'org-export-before-parsing-hook hook))))
     (error (progn
              (-each ,hooks (lambda (hook) (remove-hook 'org-export-before-parsing-hook hook)))
              (signal (car err) (cdr err))))))

(provide 'pile-utils)

;;; pile-utils.el ends here

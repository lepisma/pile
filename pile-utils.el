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
(require 's)

(defun pile--name-to-id (name)
  (s-replace-all '((" " . "-")) (downcase (s-collapse-whitespace (s-trim name)))))

(defun pile--at-header? ()
  "Return if at a header or empty line"
  (let ((line-text (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (or (s-equals? line-text "")
        (and (s-starts-with? "#+" line-text)
             (not (s-starts-with? "#+begin" (downcase line-text)))))))

(defun pile--file-title (file)
  "Return title for an org file"
  (with-temp-buffer
    (insert-file-contents file nil nil 1000)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:\\(.*\\)$")
      (s-trim (match-string-no-properties 1)))))

(defun pile--goto-top ()
  "Move point to the top of file just after the headers"
  (goto-char (point-min))
  (if (search-forward "#+SETUPFILE:" nil t)
      (progn
        (while (pile--at-header?) (next-line))
        (previous-line)
        (insert "\n"))
    (signal 'error (format "SETUPFILE line not found in %s." buffer-file-name))))

(defun pile-get-project-from-file (file-name)
  "Return project from file-name"
  (-find (lambda (pj) (s-starts-with? (oref pj :input-dir) file-name)) pile-projects))

(defun pile-get-project (name)
  "Get project by name"
  (-find (lambda (pj) (string-equal name (oref pj :name))) pile-projects))

(defun pile-read-options ()
  "Read options from org file"
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+PILE:\\(.*\\)$" nil t)
      (->> (match-string-no-properties 1)
         s-trim
         (s-split " ")
         (-map (lambda (kv) (-let [(k . v) (s-split ":" kv)] (cons (intern k) (read v)))))))))

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

(defun pile-make-pre-hooks-form ()
  "Make org-export style hooks from pile hooks and return the symbol list."
  (let ((fn-names (loop for i from 1 to (length pile-pre-publish-hook) collect (gensym "pile-hook-fn"))))
    `(progn
       ,@(loop for fn in pile-pre-publish-hook
               for fn-name in fn-names
               collect `(defun ,fn-name (_export-backend)
                          (funcall ',fn)))
       ',fn-names)))

(defmacro with-pile-hooks (&rest body)
  "Run body with pile related export hooks set."
  (let* ((add-forms `((dolist (hook pre-hooks)
                        (add-hook 'org-export-before-parsing-hook hook))
                      (dolist (hook pile-post-publish-hook)
                        (add-hook 'org-publish-after-publishing-hook hook))))
         (remove-forms `((dolist (hook pre-hooks)
                           (remove-hook 'org-export-before-parsing-hook hook)
                           (unintern hook nil))
                         (dolist (hook pile-post-publish-hook)
                           (remove-hook 'org-publish-after-publishing-hook hook)))))
    `(let ((pre-hooks (eval (pile-make-pre-hooks-form))))
       (condition-case err
           (progn ,@add-forms ,@body ,@remove-forms)
         (error (progn ,@remove-forms (signal (car err) (cdr err))))))))

(defmacro pile-when-type (project-types &rest body)
  "Run the body form when the current buffer type is from one of the given project types."
  (declare (indent defun))
  `(let ((pj (pile-get-project-from-file (buffer-file-name))))
     (when (member (oref pj :type) ,project-types)
       ,@body)))

(provide 'pile-utils)

;;; pile-utils.el ends here

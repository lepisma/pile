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

(require 'eieio)
(require 'dash)
(require 'f)
(require 's)

(defun pile-stringify-text (text)
  "Remove markups from given text, including Org mode HTML macros."
  (let (cleaned-text)
    ;; First, remove HTML tags
    (setq cleaned-text (s-replace-regexp "<.*?>" "" text))
    ;; Then, handle Org mode HTML macros like @@html:<span style=\"color: gray\">@@
    ;; This regex captures the content inside @@html:...@@ and removes the entire macro.
    (setq cleaned-text (s-replace-regexp "@@html:[^@]*@@" "" cleaned-text))
    ;; Finally, remove forward slashes
    (setq cleaned-text (s-replace-regexp "/" "" cleaned-text))))

(defun pile-stringify-title ()
  "Remove markups from current HTML buffer title, including Org mode HTML macros."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "<title>\\(.*\\)</title>" nil t)
      (let ((old-title (match-string-no-properties 1)))
        (replace-match (pile-stringify-text old-title) nil nil nil 1)))))

(defmacro pile-temp-open (filename &rest body)
  "Temporary open a file and work there. When done, keep the file
open if we had it already open, else close."
  (declare (indent defun))
  (let ((old-buffer (gensym "buf"))
        (new-buffer (gensym "buf")))
    `(let ((,old-buffer (find-buffer-visiting ,filename)))
       (if ,old-buffer
           (with-current-buffer ,old-buffer
             ,@body)
         (let ((,new-buffer (find-file-noselect ,filename)))
           (unwind-protect
             (with-current-buffer ,new-buffer
               ,@body)
             (kill-buffer ,new-buffer)))))))

(defun pile--name-to-id (name)
  (s-replace-all '((" " . "-")) (downcase (s-collapse-whitespace (s-trim name)))))

(defun pile--at-header? ()
  "Return if at a header or empty line"
  (let ((line-text (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (or (s-equals? line-text "")
        (and (s-starts-with? "#+" line-text)
             (not (s-starts-with? "#+begin" (downcase line-text)))))))

(defun pile--file-title (file)
  "Return title for an org FILE."
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

(defun pile-get-project-from-file (file)
  "Return project from FILE."
  (-find (lambda (pj) (s-starts-with? (oref pj :input-dir) file)) pile-projects))

(defun pile-dir-blog-year? (dir)
  "Tell if the DIR is year entry for a blog project.

This is helpful for `pile-get-static-items'."
  (let ((parent (f-parent dir)))
    ;; Check that parent of this has an index
    (and (or (f-exists? (f-join parent "index.org"))
             (f-exists? (f-join parent "index.html")))
         ;; Then check that this directory is named as an year
         (s-matches? "[[:digit:]]\\{4\\}" (f-base dir))
         ;; Then check if its children are all month directories
         (-all? (lambda (entry)
                  (s-matches? "[[:digit:]]\\{2\\}" (f-base entry)))
                (f-entries dir)))))

(defun pile-get-static-items (file)
  "Return static items for accompanying FILE.

FILE can be Org mode (input) or HTML file (output)."
  (let ((parent (f-parent file)))
    (->> (f-entries parent)
         ;; Skip org or html files since they are usually inputs and outputs
         (-remove (lambda (entry) (s-matches? "\\.\\(org\\|html\\)$" entry)))
         ;; Remove sibling directories that are housing an index.{org|html}
         ;; since they are other pages which will be handled separately
         (-remove (lambda (entry) (and (f-dir? entry)
                                       (or (f-exists? (f-join entry "index.org"))
                                           (f-exists? (f-join entry "index.html"))
                                           ;; Remove directories that are date paths for blog projects
                                           (pile-dir-blog-year? entry))))))))

(defun pile-clean-up-parents (path)
  "Delete parent directories for the given path if they are all
empty."
  (let ((parent (f-parent path)))
    (when (f-empty? parent)
      (f-delete parent)
      (pile-clean-up-parents parent))))

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
         (-map (lambda (kv) (-let [(k v) (s-split ":" kv)] (cons (intern k) (read v)))))))))

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

(defmacro with-pile-hooks (&rest body)
  "Run body with pile related export hooks set up. Hooks are
removed after execution."
    `(unwind-protect
         (progn
           (dolist (hook pile-pre-publish-hook)
             (add-hook 'org-export-before-parsing-hook hook t))
           (dolist (hook pile-post-publish-hook)
             (add-hook 'org-publish-after-publishing-hook hook t))
           ,@body)
       (dolist (hook pile-pre-publish-hook)
         (remove-hook 'org-export-before-parsing-hook hook))
       (dolist (hook pile-post-publish-hook)
         (remove-hook 'org-publish-after-publishing-hook hook))))

(defmacro pile-when-project-type (pj project-types &rest body)
  "Run body form when given project is one of the types."
  (declare (indent defun))
  `(let ((type-names (mapcar (lambda (pt) (intern (format "pile-project-%s" pt))) ,project-types)))
     (when (member (type-of ,pj) type-names)
       ,@body)))

(defmacro pile-when-type (project-types &rest body)
  "Run the body form when the current buffer type is from one of
the given project types."
  (declare (indent defun))
  `(pile-when-project-type (pile-get-project-from-file (buffer-file-name)) ,project-types
     ,@body))

(provide 'pile-utils)

;;; pile-utils.el ends here

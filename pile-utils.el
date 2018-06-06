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

(defun pile--file-title (file)
  "Return title for an org file"
  (second (s-split "TITLE: " (-find (-cut s-starts-with? "#+TITLE:" <>)
                                    (s-split "\n" (f-read-text file))))))

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

(defun pile--parse-option (text)
  (-map (lambda (kv)
          (-let [(k . v) (s-split ":" kv)]
            (cons k (read v))))
        (s-split " " text)))

(defun pile-read-options ()
  "Read options from org file"
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "#+PILE:")
        (let ((text (buffer-substring-no-properties (point) (line-end-position))))
          (pile--parse-option (s-trim text))))))

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
  `(let ((pre-hooks (cdr (assoc :pre ,hooks)))
         (post-hooks (cdr (assoc :post ,hooks))))
     (condition-case err
         (progn
           (-each pre-hooks (lambda (hook) (add-hook 'org-export-before-parsing-hook hook)))
           (-each post-hooks (lambda (hook) (add-hook 'org-publish-after-publishing-hook hook)))
           ,@body
           (-each pre-hooks (lambda (hook) (remove-hook 'org-export-before-parsing-hook hook)))
           (-each post-hooks (lambda (hook) (remove-hook 'org-publish-after-publishing-hook hook))))
       (error (progn
                (-each pre-hooks (lambda (hook) (remove-hook 'org-export-before-parsing-hook hook)))
                (-each post-hooks (lambda (hook) (remove-hook 'org-publish-after-publishing-hook hook)))
                (signal (car err) (cdr err)))))))

(provide 'pile-utils)

;;; pile-utils.el ends here

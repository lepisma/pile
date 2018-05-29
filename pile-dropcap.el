;;; pile-dropcap.el --- Dropcap hook for blog posts -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Dropcap hook for blog posts
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

(require 'org)
(require 'ox)
(require 'f)
(require 's)

(defun pile-dropcap-goto-first-char (buffer)
  (with-current-buffer buffer
    (pile--goto-top)
    ;; Now search for the next valid block
    (let ((move t))
      (while move
        (let ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((s-starts-with? "#+BEGIN" line-text)
            (progn
              (search-forward "#+END")
              (next-line)
              (goto-char (line-beginning-position))))
           ((s-starts-with? "#+" line-text)
            (next-line))
           ((string-equal "" line-text)
            (next-line))
           (t (setq move nil))))))))

(defun pile-dropcap-hook (_)
  "Function to add dropcap"
  (unless (string-equal "sitemap.org" (f-filename (buffer-file-name)))
    (pile-dropcap-goto-first-char (current-buffer))
    (let ((char (buffer-substring-no-properties (point) (+ 1 (point)))))
      (delete-char 1)
      (insert (format "@@html:<span class='dropcap'>%s</span>@@" char)))))

(provide 'pile-dropcap)

;;; pile-dropcap.el ends here

;;; pile-watermark.el --- HTML page watermarks for published pages -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;;
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

(require 'pile-utils)

(defun pile-watermark-add (text)
  "Add watermark repeated text in the background of the page.

This requires associated CSS rules to be set from `pile-theme'
repository. I should probably move these things together some
time."
  (pile--goto-top)
  (insert "#+BEGIN_EXPORT html\n")
  (insert "<div class=\"watermark\">" text "</div>\n")
  (insert "#+END_EXPORT"))

(provide 'pile-watermark)

;;; pile-watermark.el ends here

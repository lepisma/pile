;;; pile-serve.el --- Server for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Server for pile
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

(require 'w)

(defun pile-serve ()
  "Start server for pile"
  (interactive)
  (w-start pile-output))

(defun pile-serve-stop ()
  "Stop pile server"
  (interactive)
  (let ((wi (w-dir-live-p pile-output)))
    (if wi (w-kill wi))))

(provide 'pile-serve)

;;; pile-serve.el ends here

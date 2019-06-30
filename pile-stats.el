;;; pile-stats.el --- Stats and counts for pile -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Stats and counts for pile
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

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)

(defvar pile-stats-lm-pad-tokens '("SOS" . "EOS")
  "Starting and ending token symbols")

(defun pile-stats--pad-text (text n-pad)
  (let ((start-tok (car pile-stats-lm-pad-tokens))
        (end-tok (cdr pile-stats-lm-pad-tokens)))
    (string-join `(,@(-repeat n-pad start-tok) ,text ,@(-repeat n-pad end-tok)) " ")))

(defun pile-stats-tokenize (text)
  "Simple whitespace tokenizer."
  (s-split-words text))

(defun pile-stats-count-n-grams (n text table)
  (let* ((tokens (pile-stats-tokenize text))
         (i 0)
         (window (-take n (-drop i tokens))))
    (while (= (length window) n)
      (ht-set table window (+ 1 (ht-get table window 0)))
      (cl-incf i)
      (setq window (-take n (-drop i tokens))))
    table))

(defun pile-stats-build-lm (n texts &optional higher-counts)
  "Create an statistical n-gram LM. We don't do any smoothing
here, just raw counting. TEXTS here is a list of plain strings. N
defines the max order of n-grams. Returned LM is a list of count
tables going from 1 to N."
  (if (zerop n)
      higher-counts
    (let ((padded-texts (mapcar (lambda (text) (pile-stats--pad-text text (- n 1))) texts))
          (counts (ht (:order n))))
      (dolist (text padded-texts)
        (pile-stats-count-n-grams n text counts))
      (pile-stats-build-lm (- n 1) texts (cons counts higher-counts)))))

(provide 'pile-stats)

;;; pile-stats.el ends here

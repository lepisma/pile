(require 'pile-tags)
(require 'pile-utils)


(defmacro with-string-in-buffer (string &rest body)
  (declare (indent defun))
  `(with-temp-buffer
     (insert ,string)
     (goto-char (point-min))
     ,@body))

(describe "Option parsing"
  (it "works"
    (expect
     (with-string-in-buffer
       "#+PILE: draft:nil dropcap:t"
       (pile-read-options))
     :to-equal '((draft . nil) (dropcap . t)))))

(describe "Tags parsing"
  (it "works"
    (expect
     (with-string-in-buffer
       "#+TAGS: hello-world, testing,this"
       (pile-tags-parse-buffer))
     :to-equal '("hello-world" "testing" "this"))))

(defmacro with-string-in-buffer (string &rest body)
  (declare (indent defun))
  `(with-temp-buffer
     (insert ,string)
     (goto-char (point-min))
     ,@body))

(defmacro with-string-in-file (string &rest body)
  "Dump string to a file and bind `fname' to the path. Note that
deletion of the temporary file is not guaranteed on return."
  (declare (indent defun))
  `(let ((fname ,(make-temp-file "pile-test-file")))
     (f-write-text ,string 'utf-8 fname)
     (let ((retval ,@body))
       (delete-file fname)
       retval)))

(provide 'test-utils)

(push "test" load-path)
(require 'test-utils)

(require 'f)
(require 'pile-tags)
(require 'pile-utils)

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

(describe "Title parsing"
  (it "works"
    (expect
     (with-string-in-file
       "#+TITLE: this is the title"
       (pile--file-title fname))
     :to-equal "this is the title")))

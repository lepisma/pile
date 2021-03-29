;; -*- lexical-binding: t -*-

(push "test" load-path)
(require 'test-utils)

(require 'f)
(require 'pile-tags)
(require 'pile-utils)
(require 'pile)

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
     :to-equal "this is the title"))

  (it "is case insensitive (for key)"
    (expect
     (with-string-in-file
       "#+titLe: this is the title"
       (pile--file-title fname))
     :to-equal "this is the title")))

(describe "Publishing"
  :var (pile-output-dir)

  (before-all
    (setq pile-output-dir (f-join temporary-file-directory (make-temp-name "pile-test-")))
    (setq pile-projects (list (pile-project-wiki :name "test-wiki"
                                                 :input-dir "./test/test-input/wiki"
                                                 :output-dir (f-join pile-output-dir "wiki")))))

  (after-all
    (f-delete pile-output-dir t))

  (it "works for dummy wiki project"
    (pile-setup)
    (mkdir (f-join pile-output-dir "wiki") t)
    (expect (pile-project-publish (car pile-projects) t) :not :to-throw)
    (expect (f-entries (f-join pile-output-dir "wiki")) :to-have-same-items-as (list (f-join pile-output-dir "wiki" "index.html")))))

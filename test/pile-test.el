;; -*- lexical-binding: t -*-

(push "test" load-path)
(require 'test-utils)

(require 'f)
(require 'pile-tags)
(require 'pile-utils)
(require 'pile)
(require 'ert)
(require 's)

(ert-deftest pile-stringify-title-basic-html ()
  "Test basic HTML tag removal from title."
  (with-temp-buffer
    (insert "<html><head><title>My <span style=\"color:red;\">Awesome</span> Title</title></head><body></body></html>")
    (pile-stringify-title)
    (should (string-match-p "<title>My Awesome Title</title>" (buffer-string)))))

(ert-deftest pile-stringify-title-with-slashes ()
  "Test removal of forward slashes from title."
  (with-temp-buffer
    (insert "<html><head><title>Path/To/My/File</title></head><body></body></html>")
    (pile-stringify-title)
    ;; The expected result should be "PathToMyFile" if all slashes are removed.
    (should (string-match-p "<title>PathToMyFile</title>" (buffer-string)))))

(ert-deftest pile-stringify-title-with-org-macros ()
  "Test removal of Org mode HTML macros from title."
  (with-temp-buffer
    (insert "<html><head><title>The Urge to Sh@@html:<span style=\"color: gray\">@@o@@html:</span>@@rt@@html:<span style=\"color: gray\">@@e@@html:</span>@@n</title></head><body></body></html>")
    (pile-stringify-title)
    (should (string-match-p "<title>The Urge to Shorten</title>" (buffer-string)))))

(ert-deftest pile-stringify-title-combined-case ()
  "Test combined HTML tags, slashes, and Org macros."
  (with-temp-buffer
    (insert "<html><head><title>A <a href=\"#\">Mixed/Bag</a> of @@html:<b>@@HTML@@html:</b>@@/Org</title></head><body></body></html>")
    (pile-stringify-title)
    (should (string-match-p "<title>A MixedBag of HTMLOrg</title>" (buffer-string)))))

(ert-deftest pile-stringify-title-no-title ()
  "Test case where no title tag is present."
  (with-temp-buffer
    (insert "<html><head></head><body><h1>No Title Here</h1></body></html>")
    (pile-stringify-title)
    ;; The buffer content should remain unchanged
    (should (string= (buffer-string) "<html><head></head><body><h1>No Title Here</h1></body></html>")))
  (with-temp-buffer
    (insert "Some random text without HTML.")
    (pile-stringify-title)
    ;; The buffer content should remain unchanged
    (should (string= (buffer-string) "Some random text without HTML."))))

(ert-deftest pile-stringify-title-empty-title ()
  "Test case with an empty title tag."
  (with-temp-buffer
    (insert "<html><head><title></title></head><body></body></html>")
    (pile-stringify-title)
    (should (string-match-p "<title></title>" (buffer-string)))))

(ert-deftest pile-stringify-title-nested-org-macros ()
  "Test nested or complex Org macros (should still clean)."
  (with-temp-buffer
    (insert "<html><head><title>Nested @@html:<i>@@One@@html:</i>@@ and @@html:<span class=\"inner\">@@Two@@html:</span>@@</title></head><body></body></html>")
    (pile-stringify-title)
    (should (string-match-p "<title>Nested One and Two</title>" (buffer-string)))))

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

(require 'pile-utils)

(describe "Option parsing"
  (it "works"
    (expect (pile--parse-option "draft:nil dropcap:t") :to-equal '((draft . nil) (dropcap . t)))))

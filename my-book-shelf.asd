(defsystem "my-book-shelf"
  :version "0.1.0"
  :author "Masahiko Hamazawa"
  :license "LLGPL"
  :depends-on (:cl-dbi)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "My book shelf"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "my-book-shelf/tests"))))

(defsystem "my-book-shelf/tests"
  :author ""
  :license ""
  :depends-on ("my-book-shelf"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for my-book-shelf"

  :perform (test-op (op c) (symbol-call :rove :run c)))

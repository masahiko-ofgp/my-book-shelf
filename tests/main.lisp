(defpackage my-book-shelf/tests/main
  (:use :cl
        :my-book-shelf
        :rove))
(in-package :my-book-shelf/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :my-book-shelf)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

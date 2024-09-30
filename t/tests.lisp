(in-package :cl-user)

(defpackage #:web-lisp-tests
  (:use #:cl #:fiveam)
  (:export   #:run-tests))

(in-package #:web-lisp-tests)

(def-suite all-tests
    :description "The master suite of all quasiRPG tests.")

(in-suite all-tests)

(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3)))
  (is (= 53 (+ 2 3))))

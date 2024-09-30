(in-package :cl-user)

(defpackage #:web-lisp-tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package #:web-lisp-tests)

(def-suite all-tests
           :description "The master suite of all quasiRPG tests.")

(in-suite all-tests)

(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3)))
  (is (= 4 (+ 2 2))))

(test conf-tests
  "Test the conf"
  (let ((local-conf (web-lisp-conf::read-local-conf "t/conf/"))
        (conf (web-lisp-conf::read-conf "t/conf/")))
    (is (equalp
         '((:GLOBAL (:BIND-PORT . 8001) (:FOO . "bar")) (:DB (:USERNAME . "user")))
         local-conf))
    (is (equalp
         '((:GLOBAL (:BIND-PORT . 8000) (:BIND-ADDRESS . "127.0.0.1")))
         conf))
    (is (equalp
         '( (:DB (:USERNAME . "user"))   (:GLOBAL (:FOO . "bar") (:BIND-PORT . 8001) (:BIND-ADDRESS . "127.0.0.1")) )
         (web-lisp-conf::merge-config-lists conf local-conf)))))


(fiveam:run! 'conf-tests)
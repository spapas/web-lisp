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

;; A conf would be a nested alist; i.e
;; ((out1 -> (in11 -> v11, in12 -> v12), (out2 -> (in21 -> v21, in22 -> v22)))
;; To be able to test our conf reading functionality we need to sort these alists

(defun conf-sort-int (el)
  "Sort internal list of a conf"
  (let ((k (car el))
        (v (sort (cdr el) #'string-lessp :key #'car)))
    (cons k v)))

(defun conf-sort-out (conf)
  "Sort external list of a conf"
  (let ((conf1 (mapcar #'conf-sort-int conf)))
    (sort conf1 #'string-lessp :key #'car)))

(defun conf-eq (c1 c2)
  "Test confs for equality"
  (let ((s-c1 (conf-sort-out c1))
        (s-c2 (conf-sort-out c2)))
    (equalp s-c1 s-c2)))

(test conf-eq-tests
  "Test the conf-eq functionality"
  (is (equalp
       (conf-sort-int '(:A (:F . "bar") (:B . 8001)))
       '(:A (:B . 8001) (:F . "bar"))))
  (is (equalp
       (conf-sort-int '(:A (:B . 8001) (:F . "bar")))
       (conf-sort-int '(:A (:F . "bar") (:B . 8001)))))
  (is (equalp
       (conf-sort-out '((:Z (:D . 9001) (:A . "X")) (:A (:B . 8001))))
       '((:A (:B . 8001)) (:Z (:A . "X") (:D . 9001)))))
  (is (equalp
       (conf-sort-out '((:Z (:D . 9001) (:A . "X")) (:A (:B . 8001))))
       '((:A (:B . 8001)) (:Z (:A . "X") (:D . 9001))))))

(test conf-tests
  "Test the conf"
  (let* ((conf (web-lisp-conf::read-conf "t/conf/"))
         (local-conf (web-lisp-conf::read-local-conf "t/conf/"))
         (merged-conf (web-lisp-conf::merge-config-lists conf local-conf)))
    (is (conf-eq
          '((:GLOBAL (:BIND-PORT . 8000) (:BIND-ADDRESS . "127.0.0.1")))
          conf))
    (is (conf-eq
          '((:GLOBAL (:BIND-PORT . 8001) (:FOO . "bar")) (:DB (:USERNAME . "user")))
          local-conf))
    (is (conf-eq
          '((:DB (:USERNAME . "user")) (:GLOBAL (:BIND-PORT . 8001) (:BIND-ADDRESS . "127.0.0.1") (:FOO . "bar")))
          merged-conf))
    (is (string= (web-lisp-conf:get-conf :username :db merged-conf) "user"))
    (is (= (web-lisp-conf:get-conf :bind-port :global merged-conf) 8001))))


;(fiveam:run! 'conf-tests)
;(fiveam:run! 'conf-eq-tests)
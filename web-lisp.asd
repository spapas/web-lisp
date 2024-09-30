(in-package :cl-user)
(defpackage web-lisp-asd
  (:use :cl :asdf))
(in-package :web-lisp-asd)

(defsystem #:web-lisp
  :name "web-lisp"
  :description "web-lisp: a sample clisp web project"
  :version "0.0.1"
  :author "Serafeim"
  :licence "Public Domain"
  :build-operation "program-op"
  :build-pathname "web-lisp"
  :entry-point "web-lisp:main"
  ; :pathname #P"c:/progr/lisp/common-projects/web-lisp/"
  :depends-on (:alexandria
               :cl-ini
               :easy-routes
               :hunchentoot
               :spinneret)
  :serial t
  :components ((:file "conf")
               (:file "views")
               (:file "main"))
  :in-order-to ((test-op (test-op :web-lisp-tests))))


#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem #:web-lisp/tests
  :author "Serafeim"
  :description "tests web-lisp"
  :depends-on (:web-lisp :fiveam)
  :components ((:module "t"
                        :components
                        ((:file "tests")
                         (:file "runner"))))
  :perform (test-op (op c) (symbol-call
                            :fiveam
                            :run!
                            :web-lisp-tests)))

(in-package :cl-user)
(defpackage #:web-lisp-asd
  (:use #:cl #:asdf))
(in-package #:web-lisp-asd)

(defsystem #:web-lisp
  :name "web-lisp"
  :description "web-lisp: a sample clisp web project"
  :version "0.0.1"
  :author "Serafeim"
  :licence "Public Domain"
  :build-operation "program-op"
  :build-pathname "web-lisp"
  :entry-point "web-lisp:main"
  :depends-on (:alexandria
               :babel
               :cl-ini
               :cl-who
               :cl-forms
               :cl-forms.who
               :cl-forms.who.bootstrap
               :cl-migratum
               :cl-migratum.provider.local-path
               :cl-migratum.driver.dbi
               :dbi
               :easy-routes
               :hunchentoot
               :ironclad
               :s-base64
               :spinneret
               :split-sequence)
  :serial t
  :components ((:file "conf")
               (:file "misc")
               (:module "db"
                        :components
                        ((:file "package")
                         (:file "dao")
                         (:file "base")
                         (:file "dbq")
                         (:file "migrate")))
               (:file "auth")
               (:file "main")
               (:module "views"
                        :components
                        ((:file "helpers")
                         (:file "auth")
                         (:file "main"))))
  :in-order-to ((test-op (test-op :web-lisp/tests))))


#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem #:web-lisp/tests
  :author "Serafeim"
  :description "tests web-lisp"
  :depends-on (:web-lisp :fiveam :uiop)
  :serial t
  :components ((:module "t"
                        :serial t
                        :components
                        ((:file "tests")
                         (:file "runner"))))
  :perform (test-op (op c) (uiop:symbol-call :fiveam :run!
                             (uiop:find-symbol*
                               '#:all-tests
                               :web-lisp-tests))))

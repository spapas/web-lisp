(in-package :cl-user)

(defpackage #:web-lisp-db
  (:use #:cl)
  (:export #:query #:exec #:connect #:run-migrate #:dynamic-query #:a>))
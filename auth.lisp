(in-package :cl-user)

(defpackage #:web-lisp-auth
  (:use :cl)
  (:local-nicknames (#:ht #:hunchentoot))
  (:export #:authenticate #:login))

(in-package #:web-lisp-auth)

(defun authenticate (username password)
  "Authenticate a user"
  (if (and (string= username "root")
           (string= password "123"))
      t
      nil))

(defun do-login (username)
  "Do the login by assigning the username to the session"
  (let ((session (ht:start-session)))
    (setf (ht:session-value 'username) username)
    session))
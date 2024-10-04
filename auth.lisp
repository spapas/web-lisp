(in-package :cl-user)

(defpackage #:web-lisp-auth
  (:use :cl)
  (:local-nicknames (#:ht #:hunchentoot))
  (:export #:authenticate #:do-login #:do-logout #:logged-in))

(in-package #:web-lisp-auth)

(defun authenticate (username password)
  "Authenticate a user"
  (if (and (string= username "root")
           (string= password "123"))
      t
      nil))

(defun do-login (username)
  "Do the login by assigning the username to the session"
  (ht:log-message* :INFO username)
  (unless (null ht:*session*) (progn
                               (ht:remove-session ht:*session*)
                               (setf (ht:session ht:*request*) nil)))
  (ht:start-session)
  (setf (ht:session-value :username) username)
  (setf (ht:session-value :messages) nil))


(defun do-logout ()
  "Do the logout by removing the session"
  (ht:remove-session ht:*session*)
  (setf (ht:session ht:*request*) nil))

(defun logged-in ()
  "Check if the user is logged in"
  (if (null ht:*session*) nil
      (not (null (ht:session-value :username)))))


(defun create-superuser ()
  "Creates a superuser "
  (format t "Creating superuser ~A" (uiop:command-line-arguments)))
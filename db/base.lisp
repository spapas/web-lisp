(in-package :cl-user)

(defpackage #:web-lisp-db
  (:use :cl)
  (:export #:query #:exec #:connect #:run-migrate))

(in-package #:web-lisp-db)


(defvar *conn* nil "The global database connection")

(defun connect ()
  (let ((db-name (merge-pathnames
                   (web-lisp-conf:get-conf :filename :database)
                   (UIOP/OS:GETCWD))))
    (format t "*** Connecting to ~A~%" db-name)
    (setf *conn* (dbi:connect :sqlite3 :database-name db-name))
    (format t "*** *conn* is ~A~%" *conn*)))

(defun query (sql &rest params)
  (let* ((query (dbi:prepare *conn* sql))
         (query (dbi:execute query params)))
    (dbi:fetch-all query)))

(defun exec (sql &rest params)
  (dbi:do-sql *conn* sql params))

;(first (query "select * from user"))

; (let ((q (query "select id, created_on from apps_app")))
;   (dolist (r q) (format t "ID: ~A Created: ~A ~A ~%"
;                   (getf r :|id|)
;                   (getf r :|created_on|)
;                       (type-of (getf r :|created_on|)))))

; 

; (first (query "select id, created_on from apps_app"))

;(ql:quickload 'sqlite)
;(defvar *db* (sqlite:connect "db.sqlite3"))
;(format t "~A" (type-of (sqlite:execute-single *db* "select created_on from apps_app")))
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
  (format t "*** Query: ~A ~A~%" *conn* sql)
  (let* ((query (dbi:prepare *conn* sql))
         (query (dbi:execute query params)))
    (dbi:fetch-all query)))

(defun exec (sql &rest params)
  (dbi:do-sql *conn* sql params))

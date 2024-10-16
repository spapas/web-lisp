(in-package #:web-lisp-db)

(defstruct user
  id
  username
  password
  email
  last-name
  first-name
  is-active
  is-superuser)

(defun row-to-user (row)
  "Convert a row to a user DAO struct"
  (make-user :id (getf row :|id|)
             :username (getf row :|username|)
             :password (getf row :|password|)
             :email (getf row :|email|)
             :last-name (getf row :|last_name|)
             :first-name (getf row :|first_name|)
             :is-active (if (= 1 (getf row :|is_active|)) t nil)
             :is-superuser (if (= 1 (getf row :|is_superuser|)) t nil)))


(defun get-users ()
  "Get all users and return a list of user DAO structs"
  (mapcar #'row-to-user
      (query "SELECT * FROM user")))


(defmacro a> (obj key)
    "Get a key from a DAO struct"
    `(slot-value ,obj (find-symbol (string ,key) :web-lisp-db)))





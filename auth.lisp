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


; (ql:quickload 'ironclad)
; (ql:quickload 's-base64)

; (ironclad:pbkdf2-check-password "123" "pbkdf2_sha256$720000$NckZFbp7CMeQ90wUzCKs3y$AkiqY7S3ampxBekGD5TgOx0Uk6VdC7AE4/tvbTiod0Y=")

; (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array "123"))

; (with-output-to-string (out) 
;   (s-base64:encode-base64-bytes (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array "123") 
;                                  :salt (ironclad:ascii-string-to-byte-array "scRIdcYhttb09OmIodwIvW")
;                                  :digest 'ironclad:sha256
;                                  :iterations 720000) out ))

;37|pbkdf2_sha256$720000$scRIdcYhttb09OmIodwIvW$Auj9fYAbjw9mqvsONL9+8VALmUb7ZQztYK3RyKbpA6c=


(ql:quickload 'cl-forms)
(ql:quickload 'cl-who)
(ql:quickload 'cl-forms.who)
(ql:quickload 'cl-forms.who.bootstrap)

(cl-forms:defform fields-form (:action "/fields-post" :method :get)
                  ((name :string :value "")
                   (ready :boolean :value t)
                   (sex :choice :choices (list "Male" "Female") :value "Male")
                   (submit :submit :label "Create")))

(let ((form (cl-forms::find-form 'fields-form)))
  (forms:with-form-renderer :who
    (forms:render-form form)))

  (let ((form (forms:find-form 'fields-form)))
        (forms:handle-request form)
        (forms:with-form-field-values (name ready sex) form
            (who:with-html-output (forms.who::*html*)
               (:ul
                 (:li (who:fmt "Name: ~A" name))
                 (:li (who:fmt "Ready: ~A" ready))
                 (:li (who:fmt "Sex: ~A" sex))))))

(with-output-to-string (forms.who:*html*)
  (let ((form (forms:find-form 'fields-form)))
    (forms:with-form-renderer :who
      (forms:render-form form))))
(defvar *html* )
(let ((form (forms:find-form 'fields-form)))
	 (with-output-to-string (*html*) 
	   (spinneret:with-html
	     (let ((cl-forms.who:*html* *html*)
		   (spinneret:*html* *html*))
	       (:h1 "This is my form")
	       (forms:with-form-renderer :who
		 (forms:render-form form))
	       (:ul
		(loop for i from 1 to 10
		      do (:li (princ-to-string i))))))))

(defvar *db* (clsql:connect '("db.sqlite3") :database-type :sqlite3))
(defvar *query* "SELECT * FROM users WHERE age > ?")

;; Execute the query with a parameter
(defvar *results* (clsql:query "SELECT * FROM users_user WHERE id > 18" :database *db*))
;; Process and print the results
(dolist (row *results*)
  (format t "User: ~A~%" row))
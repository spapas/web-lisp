(in-package :cl-user)

(defpackage #:web-lisp-auth
  (:use :cl)
  (:local-nicknames (#:ht #:hunchentoot))
  (:export #:authenticate #:do-login #:do-logout #:logged-in))

(in-package #:web-lisp-auth)

(defparameter *iterations* 720000)

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
; 

; (ironclad:pbkdf2-check-password "123" "pbkdf2_sha256$720000$NckZFbp7CMeQ90wUzCKs3y$AkiqY7S3ampxBekGD5TgOx0Uk6VdC7AE4/tvbTiod0Y=")

; (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array "123"))

; (with-output-to-string (out) 
;   (s-base64:encode-base64-bytes (ironclad:pbkdf2-hash-password (ironclad:ascii-string-to-byte-array "123") 
;                                  :salt (ironclad:ascii-string-to-byte-array "scRIdcYhttb09OmIodwIvW")
;                                  :digest 'ironclad:sha256
;                                  :iterations 720000) out ))

;37|pbkdf2_sha256$720000$scRIdcYhttb09OmIodwIvW$Auj9fYAbjw9mqvsONL9+8VALmUb7ZQztYK3RyKbpA6c=


(defun create-superuser ()
  "Creates a superuser "
  (format t "Creating superuser ~A" (uiop:command-line-arguments)))


(defun create-random-string (&optional (n 10) (base 16))
  "Creates a random string using ironclad's strong-random function with base BASE and N digits"
  (setf crypto:*prng* (crypto:make-prng :fortuna))
  (subseq (with-output-to-string (s)
            (loop for i to n do
                    (format s "~VR" base
                      (ironclad:strong-random 100000000000))))
          0 n))

(defun str-to-bytea (str)
  "Converts a utf-8 str to a byte array"
  (babel:string-to-octets str :encoding :utf-8))

(defun hash-to-base64 (password salt iterations)
  "Encodes a password with sha256 using salt and iterations and returns it to base64"
  (with-output-to-string (out)
    (s-base64:encode-base64-bytes
      (ironclad:pbkdf2-hash-password (str-to-bytea password)
                                     :salt (str-to-bytea salt)
                                     :digest 'ironclad:sha256
                                     :iterations iterations) out)))

(defun serialize-password (password)
  "Serializes a password for save to the database"
  (let* ((salt (create-random-string 24))
         (encoded (hash-to-base64 password salt *iterations*)))
    (format nil "pbkdf2_sha256$~A$~A$~A" *iterations* salt encoded)))


(defun check-password (password serialized-hash)
  "Checks that the password received maches the hashed password from the db"
  (let* ((parts (split-sequence:split-sequence #\$ serialized-hash))
         (iterations (parse-integer (second parts)))
         (salt (third parts))
         (encoded (fourth parts))
         (new-encoded (hash-to-base64 password salt iterations)))
    (equal encoded new-encoded)))


(defun do-register (username password)
  "Do the register by creating a new user with username and password to the session"
  (ht:log-message* :INFO (format nil "Creating a new user with username ~a" username)))

(in-package :cl-user)

(defpackage #:web-lisp-views
  (:use :cl)
  (:local-nicknames (#:sp #:spinneret)
                    (#:ht #:hunchentoot)))

(in-package #:web-lisp-views)


(defun nav ()
  (sp:with-html (:div "NAV1")))

(defun footer ()
  (sp:with-html (:footer :style "color: red" ("Last login: ~A" *last-login*))))

(defun styles ()
  (sp:with-html (:link :href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" :rel "stylesheet" :integrity "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" :crossorigin "anonymous")))

(defmacro with-page ((&key title) &body body)
  `(sp:with-html-string
    (:doctype)
    (:html
     (:head
      (:title ,title)
      (styles))
     (:body (nav) ,@body (footer)))))

(defparameter *shopping-list*
              '("Atmospheric pond1s"
                "Electric gumption socks"
                "Mrs. Leland's embyronic television combustion"
                "Savage gymnatic aggressors"
                "ασδα1"
                "Intravenous retribution champions"))

(defparameter *user-name* "John Q. Lisper")

(defparameter *last-login* "12th 1")

(easy-routes:defroute foo ("/foo/:x") (y &get z)
  (format nil "x: ~a y: ~a z: ~a" x y z))

(easy-routes:defroute admin ("/admin") (y &get z)
  (format nil " ~a y: ~a z: ~a" (ht:start-session) y z))


(defun shopping-list ()
  (with-page (:title "Home page")
             (:header :class "container-fluid"
                      (:h1 "Home page"))
             (:section
              ("~A, here is *your* shopping list: " *user-name*)
              (:ol (dolist (item *shopping-list*)
                     (:li (1+ (random 110)) item))))))

(easy-routes:defroute hm ("/") ()
  (shopping-list))

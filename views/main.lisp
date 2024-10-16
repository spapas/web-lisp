(in-package #:web-lisp-views)


(defun shopping-list ()
  (with-page (:title "Home page")
    (:header
     (:h1 "Home page"))
    (:section
     (if (web-lisp-auth:logged-in)
         (:p (format nil "You are logged in as ~A." (ht:session-value :username)))
         (:p "You are not logged in.")))))

(easy-routes:defroute home ("/") ()
  (shopping-list))

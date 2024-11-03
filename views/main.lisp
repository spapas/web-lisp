(in-package #:web-lisp-views)


(easy-routes:defroute home ("/") ()
                      (with-page (:title "Home page")
                                 (:header
                                  (:h1 "Home page"))
                                 (:section
                                  (if (web-lisp-auth:logged-in)

                                      (:span "You are logged in as " (:b (ht:session-value :username)) ".")
                                      ;(:raw (format nil "You are logged in as <b>~A</b>." (ht:session-value :username)))
                                      (:p "You are not logged in.")))))

(in-package #:web-lisp-views)


(defparameter *shopping-list*
              '("Atmospheric pond1s"
                "Savage gymnatic aggressors"
                "ασδα1"
                "Intravenous retribution champions"))


(defun shopping-list ()
  (with-page (:title "Home page")
    (:header
     (:h1 "Home page"))
    (:section
     ("~A, here is *your* shopping list: " "KOKO")
     (:ol (dolist (item *shopping-list*)
            (:li (1+ (random 110)) item))))))

(easy-routes:defroute home ("/") ()
  (shopping-list))





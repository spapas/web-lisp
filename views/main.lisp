(in-package #:web-lisp-views)


(defparameter *shopping-list*
              '("Atmospheric pond1s"
                "Savage gymnatic aggressors"
                "ασδα1"
                "Intravenous retribution champions"))

(defparameter *user-name* "John Q. Lisper")

(defparameter *last-login* "12th 1")

(defun shopping-list ()
  (with-page (:title "Home page")
    (:header
     (:h1 "Home page"))
    (:section
     ("~A, here is *your* shopping list: " *user-name*)
     (:ol (dolist (item *shopping-list*)
            (:li (1+ (random 110)) item))))))

(easy-routes:defroute home ("/") ()
  (shopping-list))


(easy-routes:defroute login ("/login/" :method :get) ()
  (with-page (:title "Σύνδεση")

    (:form :style "max-width: 400px; margin: auto;" :method "post" :action ""
           (:h1 :class "h3 mb-3 fw-normal" "Συνδεθείτε")

           (:div :class "form-floating"
                 (:input :value "root" :name "username" :type "text" :class "form-control" :id "floatingInput" :placeholder "Ον. χρήστη")
                 (:label :for "floatingInput" "Ον. χρήστη"))

           (:div :class "form-floating"
                 (:input :value "123" :name "password" :type "password" :class "form-control" :id "floatingPassword" :placeholder "Κωδικός")
                 (:label :for "floatingPassword" "Κωδικός"))

           (:button :class "btn btn-primary w-100 py-2" :type "submit" "Σύνδεση"))))

(easy-routes:defroute login-post ("/login/" :method :post) (&post username password)
  (if (web-lisp-auth:authenticate username password)
      (progn
       (web-lisp-auth:do-login username)
       (add-flash-message "Επιτυχής σύνδεση!" 'success)
       (ht:redirect (easy-routes:genurl 'home)))
      (ht:redirect (easy-routes:genurl 'login))))

(easy-routes:defroute logout ("/logout/" :method :post) ()
  (web-lisp-auth:do-logout)
  (add-flash-message "Επιτυχής αποσύνδεση" 'success)
  (ht:redirect (easy-routes:genurl 'home)))

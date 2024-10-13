(in-package #:web-lisp-views)

(cl-forms:defform login-form (:action "/login/" :method :post)
                  ((username :string :value "")
                   (password :password :value "")
                   (submit :submit :label "Σύνδεση")))


(easy-routes:defroute login ("/login/" :method :get) ()
  (with-page (:title "Σύνδεση")
    (:div (:h2 "Συνδεθείτε")
          (:raw (render-form 'login-form))
          (:a :class "btn btn-secondary" :href (easy-routes:genurl 'register) "Εγγραφή"))))

(easy-routes:defroute login0 ("/login0/" :method :get) ()
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
      (progn
       (add-flash-message "Σφάλμα σύνδεσης" 'danger)
       (ht:redirect (easy-routes:genurl 'login)))))

(easy-routes:defroute logout ("/logout/" :method :post) ()
  (web-lisp-auth:do-logout)
  (add-flash-message "Επιτυχής αποσύνδεση" 'success)
  (ht:redirect (easy-routes:genurl 'home)))


(cl-forms:defform register-form (:action "/register/" :method :post)
                  ((username :string :value "")
                   (password :password :value "")
                   (password-verify :password :value "")
                   (submit :submit :label "Εγγραφή")))

(easy-routes:defroute register ("/register/" :method :get) ()
  (with-page (:title "Εγγραφή")
    (:div (:h2 "Εγγραφείτε")
          (:raw (render-form 'register-form)))))

(easy-routes:defroute register-post ("/register/" :method :post) (&post username password password-verify)
  (let ((form (forms:find-form 'register-form)))
    (forms:handle-request form)
    (ht:log-message* :INFO "KOOKO")
    (ht:log-message* :INFO (format nil "~A" (print-object form t)))
    (if (equal password password-verify)
        (progn
         (web-lisp-auth:do-login username)
         (add-flash-message "Επιτυχής εγγραφή" 'success)
         (ht:redirect (easy-routes:genurl 'home)))
        (progn
         (add-flash-message "Οι κωδικοί δε συμφωνούν" 'danger)
         (ht:redirect (easy-routes:genurl 'register))))))

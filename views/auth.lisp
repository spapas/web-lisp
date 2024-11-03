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
                  ((username :string :value "" :constraints (list (clavier:is-a-string)
                                                                  (clavier:not-blank)))
                   (password :password :value "")
                   (password-verify :password :value "")
                   (email :email :value "")
                   (first-name :password :value "")
                   (last-name :password :value "")
                   (submit :submit :label "Εγγραφή")))

(defun render-register-form (&optional form-instance)
  (with-page (:title "Εγγραφή")
    (:div (:h2 "Εγγραφείτε")
          (:raw
           (if form-instance
               (render-form-instance form-instance)
               (render-form 'register-form))))))

(easy-routes:defroute register ("/register/" :method :get) ()
  (render-register-form))

(defun register-form-errors (form)
  (if (forms::validate-form form)
      (forms::with-form-field-values (username password password-verify) form
        (cond ((not (equal password password-verify))
                (forms:add-form-error 'password-verify "Οι κωδικοί δεν ταιριάζουν" form))
              ((equal username "root")
                (forms:add-form-error 'username "Το όνομα χρήστη υπάρχει ήδη" form))
              (t nil)))
      form))

(easy-routes:defroute register-post ("/register/" :method :post) ()
  (let ((form (forms:find-form 'register-form)))
    (forms:handle-request form)
    (if (register-form-errors form)
        (render-register-form form)
        (forms::with-form-field-values (username password email first-name last-name) form
          (progn
           (ht:log-message* :INFO (format nil "Creating a new user with username ~a" username))
           (web-lisp-auth:do-register username password email first-name last-name)
           (web-lisp-auth:do-login username)
           (add-flash-message "Επιτυχής εγγραφή" 'success)
           (ht:redirect (easy-routes:genurl 'home)))))))


(defun render-boolean (value)
  (if value "Ναι" "Όχι"))


(defun forbidden (&optional (message "Forbidden"))
  (setf (ht:return-code*) ht:+http-forbidden+)
  message)

(defun @superuserp (next)
  (if (web-lisp-auth:superuserp)
      (funcall next)
      (forbidden)))

(easy-routes:defroute
    users ("/users/"
           :method :get
           :decorators (@superuserp)) ()
  (with-page (:title "Χρήστες")
    (:h2 "Χρήστες")
    (:table :class "table"
            (:tr (:th "ID")
                 (:th "Όνομα χρήστη")
                 (:th "Επώνυμο")
                 (:th "Όνομα")
                 (:th "Email")
                 (:th "Ενεργός")
                 (:th "Διαχειριστής"))
            (loop for user in (web-lisp-db::get-users)
                  collect
                    (:tr (:td (a> user 'id))
                         (:td (a> user 'username))
                         (:td (a> user 'last-name))
                         (:td (a> user 'first-name))
                         (:td (a> user 'email))
                         (:td (render-boolean (a> user 'is-active)))
                         (:td (render-boolean (a> user 'is-superuser))))))))



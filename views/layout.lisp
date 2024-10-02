(in-package :cl-user)

(defpackage #:web-lisp-views
  (:use :cl)
  (:local-nicknames (#:sp #:spinneret)
                    (#:ht #:hunchentoot)))

(in-package #:web-lisp-views)

(defun add-flash-message (message &optional (type :info))
  (push (list type message) (ht:session-value :flash-messages)))

(defun get-messages ()
  (let ((messages (copy-tree (ht:session-value :flash-messages))))
    (setf (ht:session-value :flash-messages) nil)

    messages))

(defun nav ()
  (sp:with-html
    (:nav :class "navbar navbar-expand-md navbar-dark bg-dark mb-4"
          (:div :class "container-fluid"
                (:a :class "navbar-brand" :href (easy-routes:genurl 'home) "Home")
                (:button :class "navbar-toggler collapsed"
                         :type "button"
                         :data-bs-toggle "collapse"
                         :data-bs-target "#navbarCollapse"
                         :aria-controls "navbarCollapse"
                         :aria-expanded "false"
                         :aria-label "Toggle navigation"
                         (:span :class "navbar-toggler-icon"))
                (:div :class "navbar-collapse collapse" :id "navbarCollapse"
                      (:ul :class "navbar-nav me-auto mb-2 mb-md-0"
                           (:li :class "nav-item"
                                (if (web-lisp-auth:logged-in)
                                    (:form :method "post" :action (easy-routes:genurl 'logout)
                                           (:button :class "nav-link" :type "submit"
                                                    (concatenate 'string (ht:session-value :username) "| Logout")))
                                    (:a :class "nav-link" :aria-current "page" :href (easy-routes:genurl 'login) "Login")))
                           (:li :class "nav-item"
                                (:a :class "nav-link" :href "#" "Link"))))))))

(defun show-messages ()
  (let ((messages (get-messages)))
    (unless (null messages)
      (sp:with-html
        (:div :class "container-fluid"
              (dolist (message messages)
                (:div
                 :class (concatenate 'string "alert alert-" (format nil "~(~a~)" (car message)) " alert-dismissible fade show")
                 :role "alert"
                 (:ul :style "margin-bottom: 0; margin-top: 0;"
                      (:li (second message)))
                 (:button :id "messages-alert" :type "button" :class "btn-close" :data-bs-dismiss "alert" :aria-label "Close"))))))))

(defun footer ()
  (sp:with-html (:div :class "container-fluid"
                      (:footer :class "d-flex flex-wrap justify-content-between align-items-center py-3 my-4 border-top"
                               (:p :class "col-md-4 mb-0 text-body-secondary" (web-lisp::debug-session ht:*session*))
                               (:a :href "/" :class "col-md-4 d-flex align-items-center justify-content-center mb-3 mb-md-0 me-md-auto link-body-emphasis text-decoration-none")
                               (:ul :class "nav col-md-4 justify-content-end"
                                    (:li :class "nav-item" (:a :href "#" :class "nav-link px-2 text-body-secondary" "Home"))
                                    (:li :class "nav-item" (:a :href "#" :class "nav-link px-2 text-body-secondary" "FAQs"))
                                    (:li :class "nav-item" (:a :href "#" :class "nav-link px-2 text-body-secondary" "About")))))))

(defun styles ()
  (sp:with-html (:link
                 :href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"
                 :rel "stylesheet"
                 :integrity "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"
                 :crossorigin "anonymous")))

(defun js ()
  (sp:with-html (:script
                 :src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"
                 :integrity "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz"
                 :crossorigin "anonymous")))

(defmacro with-page ((&key title) &body body)
  `(sp:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (styles)
       (js))
      (:body (nav)
             (show-messages)
             (:div :class "container-fluid"
                   ,@body)
             (footer)))))

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

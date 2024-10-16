(in-package :cl-user)

(defpackage #:web-lisp-views
  (:use :cl)
  (:import-from :web-lisp-db #:a>)
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
                                (:a :class "nav-link" :href "#" "Link")))
                      (:ul :class "navbar-nav  mb-2 mb-md-0"
                           (:li :class "nav-item "
                                (if (web-lisp-auth:logged-in)
                                    (:form :method "post" :action (easy-routes:genurl 'logout)
                                           (:button :class "nav-link" :type "submit"
                                                    (concatenate 'string (ht:session-value :username) "| Logout")))
                                    (:a :class "nav-link" :aria-current "page" :href (easy-routes:genurl 'login) "Login")))))))))

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

(defvar *html*)


(defun render-form (form)
  (render-form-instance (cl-forms:get-form form)))


(defun render-form-instance (form)
  (with-output-to-string (*html*)
    (spinneret:with-html
      (let ((cl-forms.who:*html* *html*)
            (spinneret:*html* *html*))
        (cl-forms:with-form-theme 'cl-forms.who::bootstrap-form-theme
          (cl-forms:with-form-renderer :who
            (cl-forms:render-form form)))))))
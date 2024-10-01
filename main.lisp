(in-package :cl-user)

(defpackage #:web-lisp
  (:use :cl :web-lisp-conf)
  (:local-nicknames (#:ht #:hunchentoot))
  (:export #:main #:start #:stop))

(in-package #:web-lisp)

(setf ht:*session-secret* (get-conf :session-secret))

(defun debug-request (request)
  (let* ((uri (ht:request-uri request))
         (method (ht:request-method request)))
    (ht:log-message* :INFO
      (concatenate 'string
        "REQUEST:~%Method: " (format nil "~A" method) "~%"
        "URI: " (format nil "~A" uri)))))

(defclass slash-redirect-acceptor (easy-routes:easy-routes-acceptor)
    ()
  (:documentation "An acceptor that redirects requests without a trailing slash to the 
   same URL with a trailing slash, while supporting Easy-Routes."))

(defmethod ht:acceptor-dispatch-request ((acceptor slash-redirect-acceptor) request)
  (flet ((ends-with (string char)
                    "Checks if STRING ends with CHAR."
                    (and (> (length string) 0) (char= (char string (1- (length string))) char))))
    (let* ((uri (ht:request-uri request))
           (method (ht:request-method request)))
      (debug-request request)
      (if (and (not (equal "/" uri))
               (not (ends-with uri #\/))
               (eq :get method))
          (ht:redirect (concatenate 'string uri "/") :code 301)
          (call-next-method)))))


(defmethod ht:session-created ((acceptor slash-redirect-acceptor) (session t))
  (ht:log-message* :INFO "Session created: ~A // ~A // ~Α"
    session
    session ht:*session*
    (ht:session-id session))
  (call-next-method))

(defvar *base-acceptor* (make-instance
                            ;'easy-routes:easy-routes-acceptor
                          'slash-redirect-acceptor
                          :port (get-conf :bind-port)
                          :address (get-conf :bind-address)))

;; serve static files
(push (ht:create-folder-dispatcher-and-handler
        "/static/"
        #p"c:/progr/lisp/projects/web-lisp/static/")
      ht:*dispatch-table*)

;; Define some test routes
(ht:define-easy-handler
  (login :uri "/login") ()
  (setf (ht:content-type*) "text/html")
  "κοκο")

(ht:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (ht:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(defun start ()
  (ht:start *base-acceptor*))

(defun stop ()
  (ht:stop *base-acceptor*))

(defun main ()
  (format t "Starting acceptor")
  (terpri)
  (start)

  (flet ((cleanup ()
                  (format t "Stopping acceptor~%")
                  (ht:stop *base-acceptor*)))

    ;; Set up a handler for the SIGINT signal (Ctrl+C)
    (handler-bind ((sb-sys:interactive-interrupt
                    (lambda (condition)
                      (declare (ignore condition))
                      (cleanup) ; Call cleanup when Ctrl+C is pressed
                      (sb-ext:exit))))

      ; Join the listener-thread to avoid killing it
      (sb-thread:join-thread
        (find-if
            (lambda (th)
              (alexandria:starts-with-subseq "hunchentoot-listener" (sb-thread:thread-name th)))
            (sb-thread:list-all-threads))))))


;; Start at repl
;; (ql:quickload :web-lisp)
;; (web-lisp:start)
;; Stop
;; (web-lisp:stop)

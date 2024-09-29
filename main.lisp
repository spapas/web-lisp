(in-package :cl-user)

(defpackage #:web-lisp
  (:use :cl)
  (:local-nicknames (#:ht #:hunchentoot))
  (:export #:main #:start #:stop))

(in-package #:web-lisp)

(defvar *base-acceptor* (make-instance
                            'easy-routes:easy-routes-acceptor
                          :port 8001
                          :address "127.0.0.1"))

(ht:acceptor-address *base-acceptor*)
ht:*dispatch-table*
(type-of *base-acceptor*)

(ht:define-easy-handler
  (login :uri "/login") ()
  (setf (ht:content-type*) "text/html")
  "κοκο")

(push (ht:create-folder-dispatcher-and-handler
        "/static/"
        #p"c:/progr/")
      ht:*dispatch-table*)

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

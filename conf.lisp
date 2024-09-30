(in-package :cl-user)

(defpackage #:web-lisp-conf
  (:use :cl)
  (:export #:get-conf))

(in-package #:web-lisp-conf)


(defun read-local-conf ()
  (let ((local-conf (merge-pathnames "conf/local.ini" (UIOP/OS:GETCWD))))
    (when (probe-file local-conf)
          (cl-ini:parse-ini local-conf))))

(defun read-conf ()
  (cl-ini:parse-ini (merge-pathnames "conf/base.ini" (UIOP/OS:GETCWD))))


(defun alist-keys (alist)
  (mapcar 'car alist))

(defun merge-config-lists (li1 li2)
  (let ((li2-keys (alist-keys li2)))
    (dolist (key li2-keys)
      (if (assoc key li1)
          (let ((value2 (cdr (assoc key li2))))

            (if (atom (cdr (assoc key li1)))
                (rplacd (assoc key li1) value2)
                (rplacd (assoc key li1) (merge-config-lists (cdr (assoc key li1)) (cdr (assoc key li2))))))
          (push (assoc key li2) li1)))
    li1))

(defparameter *conf* (merge-config-lists (read-conf) (read-local-conf)))


(defun get-conf (key &optional (section :global))
  (cl-ini:ini-value *conf* key :section section))

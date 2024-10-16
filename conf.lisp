(in-package :cl-user)

(defpackage #:web-lisp-conf
  (:use :cl)
  (:export #:get-conf))

(in-package #:web-lisp-conf)

(defvar *conf-dir* "conf/")
(defvar *conf* nil "The global configuration")

;;; The base configuration is kept in a base.ini file inside /conf. The settings
;;; there can be overriden by adding a local.ini file in the same directory.
;;; The local.ini shouldn't be added to the VCS.

(defun read-conf (&optional (conf-dir *conf-dir*))
  "Read the base configuration"
  (let* ((base-conf-name (concatenate 'string conf-dir "base.ini"))
         (base-conf (merge-pathnames base-conf-name (UIOP/OS:GETCWD))))
    (cl-ini:parse-ini base-conf)))

(defun read-local-conf (&optional (conf-dir *conf-dir*))
  "Read the extra configuration"
  (let* ((local-conf-name (concatenate 'string conf-dir "local.ini"))
         (local-conf (merge-pathnames local-conf-name (UIOP/OS:GETCWD))))
    (when (probe-file local-conf)
          (cl-ini:parse-ini local-conf))))

(defun alist-keys (alist)
  "Helper function. Get the keys of an alist"
  (mapcar 'car alist))

(defun merge-config-lists (li1-in li2)
  "Merge two aconf lists: For all keys of the 2nd conf check if key exists
  in the 1st. If yes merge the elements if not copy it over. Doesn't modify
  the 1st list (uses treecopy)"
  (let ((li1 (copy-tree li1-in))
        (li2-keys (alist-keys li2)))
    (dolist (key li2-keys)
      (if (assoc key li1)
          (let ((value2 (cdr (assoc key li2))))

            (if (atom (cdr (assoc key li1)))
                (rplacd (assoc key li1) value2)
                (rplacd (assoc key li1) (merge-config-lists (cdr (assoc key li1)) (cdr (assoc key li2))))))
          (push (assoc key li2) li1)))
    li1))

;(defparameter *conf* (merge-config-lists (read-conf) (read-local-conf)))

(defun reread-conf ()
  "Reread the configuration"
  (setf *conf* (merge-config-lists (read-conf) (read-local-conf))))

(defun get-conf (key &optional (section :global) (conf-holder *conf*))
  "The base API of conf; get a value"
  (when (null conf-holder)
        (setf *conf* (merge-config-lists (read-conf) (read-local-conf))))
  (cl-ini:ini-value conf-holder key :section section))

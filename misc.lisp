(in-package :cl-user)

(defpackage #:web-lisp-misc
  (:use #:cl #:web-lisp-conf)
  (:local-nicknames (#:ht #:hunchentoot))
  (:export ensure-path-ends-with-slash))

(in-package #:web-lisp-misc)

(defun ensure-path-ends-with-slash (uri)
  (let* ((uri-parts (split-uri uri))
         (scheme (first uri-parts))
         (authority (second uri-parts))
         (path (third uri-parts))
         (query (fourth uri-parts))
         (fragment (fifth uri-parts)))
    (if (and path (not (string= path ""))
             (not (char= (char path (1- (length path))) #\/)))
        (format nil "~@[~A://~]~@[~A~]~A/~@[?~A~]~@[#~A~]"
          scheme authority path query fragment)
        uri)))

(defun split-uri (uri)
  (let ((scheme-end (search "://" uri))
        (authority-end (position #\/ uri :start (if (search "://" uri)
                                                    (+ (search "://" uri) 3)
                                                    0)))
        (query-start (position #\? uri))
        (fragment-start (position #\# uri)))
    (list
     ;; scheme
     (if scheme-end (subseq uri 0 scheme-end) nil)
     ;; authority
     (if (and scheme-end authority-end)
         (subseq uri (+ scheme-end 3) authority-end)
         nil)
     ;; path
     (subseq uri (or authority-end 0)
             (or query-start fragment-start (length uri)))
     ;; query
     (if query-start
         (subseq uri (1+ query-start)
                 (or fragment-start (length uri)))
         nil)
     ;; fragment
     (if fragment-start
         (subseq uri (1+ fragment-start))
         nil))))

(in-package #:web-lisp-db)

(ql:quickload 'cl-migratum)
(ql:quickload 'cl-migratum.provider.local-path)
(ql:quickload :cl-migratum.driver.dbi)


(defparameter *provider*
              (migratum.provider.local-path:make-provider
               (list (merge-pathnames "db/migrations/" (UIOP/OS:GETCWD)))))

(migratum:provider-list-migrations *provider*)

(defparameter *driver*
              (migratum.driver.dbi:make-driver *provider* web-lisp-db::*conn*))

(defun run-migrate ()
  (migratum:driver-init *driver*)
  (migratum:provider-init *provider*)
  (migratum:list-pending *driver*)
  (migratum:display-pending *driver*)
  (migratum:apply-pending *driver*)
  (migratum:migration-id (migratum:latest-migration *driver*))
  (migratum:migration-description (migratum:latest-migration *driver*))

  (migratum:driver-list-applied *driver*)
  (migratum:display-applied *driver*)
  ;(migratum:revert-last *driver*)
)
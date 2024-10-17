(in-package #:web-lisp-db)

(defmacro dynamic_query (&key fields tables join where order limit)
  (let ((query (gensym "query")))
    `(let ((,query "SELECT "))
       (setf ,query (concatenate 'string ,query
                      (format nil "~{~A~^, ~}"
                        (mapcar #'to-sql-identifier ',fields))))
       (setf ,query (concatenate 'string ,query
                      " FROM "
                      (format nil "~{~A~^, ~}"
                        (mapcar #'to-sql-identifier ',tables))))
       ,(when join
              `(setf ,query (concatenate 'string ,query
                              " JOIN "
                              (format nil "~A ON ~A"
                                (to-sql-identifier ',(second join))
                                (to-sql-join-condition ',(third join))))))
       ,(when where
              `(setf ,query (concatenate 'string ,query
                              " WHERE "
                              (to-sql-where-condition ',where))))
       ,(when order
              `(setf ,query (concatenate 'string ,query
                              " ORDER BY "
                              (format nil "~{~A~^, ~}"
                                (mapcar #'to-sql-order ',order)))))
       ,(when limit
              `(setf ,query (concatenate 'string ,query
                              " LIMIT "
                              (write-to-string ,limit))))

       ,query)))

(defun to-sql-identifier (expr)
  (typecase expr
    (string expr)
    (symbol (string-downcase (symbol-name expr)))
    (cons (format nil "~A ~A" (to-sql-identifier (first expr)) (to-sql-identifier (second expr))))
    (t (error "Unsupported identifier type: ~A" expr))))

(defun to-sql-value (expr)
  (typecase expr
    (string (format nil "'~A'" (replace-all expr "'" "''")))
    (number (write-to-string expr))
    (symbol (if (member expr '(null nil)) "NULL" (string-upcase (symbol-name expr))))
    (t (error "Unsupported value type: ~A" expr))))

(defun to-sql-join-condition (condition)
  (if (stringp condition)
      condition
      (format nil "~A ~A ~A"
        (to-sql-identifier (first condition))
        (to-sql-operator (second condition))
        (to-sql-identifier (third condition)))))

(defun to-sql-where-condition1 (condition)
  (format t "~A" condition)
  (cond
   ((stringp condition) condition)
   ((and (listp condition) (= (length condition) 3))
     (format nil "~A ~A ~A"
       (to-sql-identifier (first condition))
       (to-sql-operator (second condition))
       (to-sql-value (third condition))))
   ((and (listp condition) (every #'listp condition))
     (format nil "(~{~A~^ AND ~})" (mapcar #'to-sql-where-condition condition)))
   (t (error "Invalid WHERE condition format: ~A" condition))))


(defun to-sql-where-condition (condition)
  (cond
   ((stringp condition) condition)
   ((and (listp condition) (member (first condition) '(:and :or)))
     (let* ((op (string-upcase (symbol-name (first condition))))
            (clauses (mapcar #'to-sql-where-condition (rest condition))))
       (format nil (concatenate 'string "(~{~A~^ " op " ~})") clauses)))

   ((and (listp condition) (= (length condition) 3))
     (format nil "~A ~A ~A"
       (to-sql-identifier (first condition))
       (to-sql-operator (second condition))
       (to-sql-value (third condition))))

   (t (error "Invalid SQL condition format: ~A" condition))))


(defun to-sql-operator (op)
  (string-upcase
    (etypecase op
      (string op)
      (symbol (symbol-name op)))))

(defun to-sql-order (expr)
  (if (listp expr)
      (format nil "~A ~A" (to-sql-identifier (first expr)) (string-upcase (symbol-name (second expr))))
      (to-sql-identifier expr)))

(defun replace-all (string part replacement &key (test #'char=))
  "Replace all occurrences of part in string with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                      :start2 old-pos
                      :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
            when pos do (write-string replacement out)
          while pos)))

(web-lisp-db:query "select file from pragma_database_list where name='main';")

(web-lisp-db:query
  (dynamic_query :fields (u.id u.username u.email)
                 :tables ((user u))
                 ;:join (departments d (u.department_id = d.id))
                 :where (:and (u.is_active = 1)
                              (:or (u.username = "foo") (u.username = "root")))
                 :order ((u.last_name :asc))
                 :limit 100))

(dynamic_query :fields (u.id u.username u.email)
               :tables ((user u))
               ;:join (departments d (u.department_id = d.id))
               :where (:and (u.is_active = 1)
                            (:or (u.username = "foo") (u.username = "root")))
               :order ((u.last_name :asc))
               :limit 100)
(ql:quickload 'sqlite)
(defvar *db* (sqlite:connect "db.sqlite"))
(format t "~A" (sqlite:execute-single *db* "select 13./43"))

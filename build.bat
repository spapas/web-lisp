REM sbcl --non-interactive --eval "(ql:quickload :web-lisp)" --eval "(asdf:make :web-lisp)" 
sbcl --noinform --non-interactive --load web-lisp.asd ^
  --eval "(setf *compile-verbose* nil)" ^
  --eval "(with-open-file (*error-output* \"warnings.log\" :direction :output :if-exists :supersede) (asdf:load-system :web-lisp))" ^
  --eval "(asdf:make :web-lisp)"

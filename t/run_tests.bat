REM sbcl --non-interactive  --load web-lisp.asd --eval "(asdf:load-system :web-lisp/tests)" --eval "(web-lisp-tests:run-tests)" 
sbcl --noinform --non-interactive  --load web-lisp.asd ^
  --eval "(setf *compile-verbose* nil)" ^
  --eval "(with-open-file (*error-output* \"warnings.log\" :direction :output :if-exists :supersede) (asdf:load-system :web-lisp/tests))" ^
  --eval "(web-lisp-tests:run-tests)" 

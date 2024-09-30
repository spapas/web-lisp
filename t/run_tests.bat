sbcl --non-interactive --load web-lisp.asd --eval "(asdf:load-system :web-lisp/tests)" --eval "(web-lisp-tests:run-tests)" 

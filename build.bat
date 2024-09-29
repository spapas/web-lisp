REM sbcl --non-interactive --eval "(ql:quickload :clisp-sample-asdf)" --eval "(asdf:make :clisp-sample-asdf)" 
sbcl --non-interactive --load web-lisp.asd --eval "(asdf:load-system :web-lisp)" --eval "(asdf:make :web-lisp)" 

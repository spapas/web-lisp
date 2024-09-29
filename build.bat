REM sbcl --non-interactive --eval "(ql:quickload :web-lisp)" --eval "(asdf:make :web-lisp)" 
sbcl --non-interactive --load web-lisp.asd --eval "(asdf:load-system :web-lisp)" --eval "(asdf:make :web-lisp)" 

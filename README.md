# web-lisp

Sample project for a CLisp web-app.

You need to use sbcl and install quicklisp. Sample .sbcrlc (i.e C:\Users\serafeim\.sbclrc):

```lisp
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Add local-project to *local-project-directories*
(push #p"c:/progr/lisp/local-projects/" ql:*local-project-directories*)
(push #p"c:/progr/lisp/projects/" ql:*local-project-directories*)
```

Clone the project on one of your `ql:*local-project-directories*` folder. 

**To use with the repl**

```lisp
;; Load project from ql
(ql:quickload :web-lisp)
;; Start the server
(web-lisp:start)
;; Stop server
(web-lisp:stop)
```

**To run**

sbcl --load web-lisp.asd --eval "(ql:quickload :web-lisp)" --eval "(web-lisp:start)"

**To build**

Run `build.bat`


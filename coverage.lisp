#|
Run tests + generate coverage report, use sbcl --script coverage.lisp
|#
(in-package #:cl-user)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(uiop:delete-directory-tree #p"coverage/" :validate t :if-does-not-exist :ignore)

(defvar *system* "anansi")

#+sbcl
(require :sb-cover)
#+sbcl
(declaim (optimize sb-cover:store-coverage-data))

(handler-bind ((warning #'muffle-warning))
  (let ((*compile-verbose* nil)
        (*load-verbose* nil))
    (asdf:load-system *system* :force t)
    (asdf:test-system *system*))
  #+sbcl
  (sb-cover:report "coverage/"))

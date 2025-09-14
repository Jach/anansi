#|
Run tests + generate coverage report, use sbcl --script coverage.lisp
|#
(in-package #:cl-user)

(defvar *system* "anansi")

(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(uiop:delete-directory-tree #p"coverage/" :validate t :if-does-not-exist :ignore)

(handler-bind ((warning #'muffle-warning))
  (let ((*compile-verbose* nil)
        (*load-verbose* nil))
    (asdf:load-system *system* :force t)
    (asdf:test-system *system*))
  (sb-cover:report "coverage/" :form-mode :car))

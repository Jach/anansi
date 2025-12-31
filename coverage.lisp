#|
Run tests + generate coverage report, use sbcl --script coverage.lisp
Pass an additional argument, --skip-webdriver, to skip the webdriver tests.
|#
(in-package #:cl-user)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(require :uiop)

(defvar *system* "anansi")
(defvar *system2* "anansi/example")
(defvar *skip-webdriver* (find "--skip-webdriver" (uiop:command-line-arguments) :test #'equal))

;; Load the underlying test systems for both outside the coverage declaration

(ql:quickload "anansi/test")
(unless *skip-webdriver*
  (ql:quickload "anansi/web-test"))

(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

(uiop:delete-directory-tree #p"coverage/" :validate t :if-does-not-exist :ignore)

(handler-bind ((warning #'muffle-warning))
  (let ((*compile-verbose* nil)
        (*load-verbose* nil))
    (asdf:load-system *system* :force t)
    (unless *skip-webdriver*
      (asdf:load-system *system2* :force t))
    ;; We only perform the test operation on one system because either one will fiveam:run-all-tests defined so far.
    (asdf:test-system *system*)
    )
  (sb-cover:report "coverage/"))

(defpackage #:com.thejach.anansi/example
  (:export #:main
           #:start
           #:stop)
  (:use #:cl))

(in-package #:com.thejach.anansi/example)

(defun start ()
  (com.thejach.anansi/example.web:start))

(defun stop ()
  (format t "~%Stopping~%")
  (com.thejach.anansi/example.web:stop))

(defun exit-cleanly ()
  (stop)
  (uiop:quit 0))

(defun exit-with-backtrace (c)
  (stop)
  (uiop:print-condition-backtrace c :count 15)
  (uiop:quit 1))

(defun main ()
  (handler-bind
    ((serious-condition (lambda (c)
                          (typecase c
                            #+sbcl
                            (sb-sys:interactive-interrupt (exit-cleanly))
                            (t (exit-with-backtrace c))))))
    (start)
    (loop do (sleep most-positive-fixnum))))

;; If running from a repl, I suggest:
#+nil
(bt:make-thread #'main)

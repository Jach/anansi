(defpackage #:com.thejach.anansi/example
  (:local-nicknames (#:config #:com.thejach.anansi/example.config))
  (:export #:main
           #:start
           #:stop)
  (:use #:cl))

(in-package #:com.thejach.anansi/example)

(defun start (port)
  (format t "Starting http://localhost:~a -- port ~a and app-root ~a~%" port port (config:app-root))
  (com.thejach.anansi/example.web:start port))

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
    (start (config:config :server-port))
    (loop do (sleep most-positive-fixnum))))

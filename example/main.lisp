(defpackage #:com.thejach.anansi/example
  (:export #:main
           #:start
           #:stop)
  (:use #:cl))

(in-package #:com.thejach.anansi/example)

(defvar *log-lock* (bt:make-lock))

(defun logger (level msg)
  (bt:with-lock-held (*log-lock*)
    (case level
      (:info (vom:info msg))
      (:debug (vom:debug msg))
      (t (vom:info "~a (real level: ~a)" msg level)))))

(defun setup-logging ()
  "Configures the logging framework of choice to receive logs from Anansi.
   Here we use vom for simplicity, but it's kind of a bit too simple because it lacks
   thread safety or a generic log function for different levels."
  (vom:config t :debug)
  (setf com.thejach.anansi:*logger* #'logger))

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
  (setup-logging)
  (com.thejach.anansi/example.authentication:create-limiter)
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

(defpackage #:com.thejach.anansi/example.authentication
  (:use #:cl)
  (:local-nicknames (#:anansi #:com.thejach.anansi))
  (:export #:create-limiter
           #:verify-login
           #:generate-hash))

(in-package #:com.thejach.anansi/example.authentication)

(defparameter *limiter* nil)

(defun create-limiter ()
  (setf *limiter* (anansi:make-login-rate-limiter)))

(defun check-password-against-hash (password hash)
  (handler-case
    (bcrypt:password= password hash)
    (error () nil)))

(defun verify-login (user-id user-password user-hashed-password ip)
  (anansi:verify-login *limiter* user-id ip (lambda () (check-password-against-hash user-password user-hashed-password))))

(defun generate-hash (password ip)
  (anansi:verify-login *limiter* nil ip (lambda () (bcrypt:encode (bcrypt:make-password password :cost 12)))))

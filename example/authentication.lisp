(defpackage #:com.thejach.anansi/example.authentication
  (:use #:cl))

(in-package #:com.thejach.anansi/example.authentication)

(defparameter *limiter* (com.thejach.anansi:make-limiter :constant-runtime 1.0
                                                         :jitter 0
                                                         :concurrency 4
                                                         :max-wait 0.58))

;(log:config :sane2)
;(loop for i below 15
;      collect
;  (bt:make-thread (lambda ()
;                    (log:info
;                    (com.thejach.anansi:with-computation (*limiter*)
;                      (generate-hash "hello"))))
;                  )
;;  )
;*

;(bt:make-thread (lambda ()
;                  (dotimes (_ 24)
;                    (bt:make-thread (lambda (&aux res now)
;                                      (setf now (com.thejach.anansi::now-seconds))
;                                      (setf res (com.thejach.anansi:compute-result-final-status
;                                        (com.thejach.anansi:with-computation (*limiter*)
;                                          (generate-hash "hello"))))
;                                      (format t "~f ~a~%" (- (com.thejach.anansi::now-seconds) now) res)
;)))))

(defun generate-hash (password)
  (bcrypt:encode (bcrypt:make-password password :cost 12)))

(defun check-password-against-hash (password hash)
  (bcrypt:password= password hash))


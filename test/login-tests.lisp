(in-package #:anansi-tests)

(def-suite login-rate-limiter-suite)

(in-suite login-rate-limiter-suite)

(defun make-test-limiter (&key (max-attempts 3)
                               (ban-duration 5)
                               (verify-deadline 0.5))
  (make-login-rate-limiter
    :max-attempts-per-minute max-attempts
    :ban-duration-minutes ban-duration
    :cleanup-interval-minutes (/ 1.0 60) ; every second
    :constant-runtime verify-deadline
    :max-wait 0.1
    :jitter 0.1))

(test create-limiter
  "Test creation of rate limiter"
  (let ((limiter (make-test-limiter)))
    (is (not (null limiter)))
    (is (typep limiter 'login-rate-limiter))))

#+sbcl
(test maintenance-thread-gc-able
  "Verifies there's no root to the maintenance thread outside of a let block"
  (let ((lim-ptr))
    (let ((lim (make-test-limiter :verify-deadline 0.1)))
      (setf lim-ptr (sb-ext:make-weak-pointer lim))
      (is-true (sb-ext:search-roots lim-ptr :print nil)))

    (sleep 0.2)
    (is-false (sb-ext:search-roots lim-ptr :print nil))))

#+sbcl
(test maintenance-thread-garbage-collected
  "Tries to force a garbage collect after the limiter goes out of scope and have the maintenance thread end itself.
   Warning, can be a flakey test..."
  (let ((thread-name))
    (let ((lim (make-test-limiter)))
      (setf thread-name (bt:thread-name (com.thejach.anansi::.maintenance-thread lim)))
      (is-true (find-if (lambda (thread)
                          ;(str:starts-with? "login-rate-limiter-maintenance-thread" (bt:thread-name thread)))
                          (equal thread-name (bt:thread-name thread)))
                        (bt:all-threads))))
    (sb-ext:gc :full t)
    (sleep 1) ; cleanup-interval
    (sb-ext:gc :full t)
    (sleep 1) ; twice the interval
    (sb-ext:gc :full t)
    (sleep 0.5) ; almost thrice the interval
    (is-false (find-if (lambda (thread)
                         (equal thread-name (bt:thread-name thread)))
                       (bt:all-threads)))))

(test basic-verification
  "Test basic login verification"
  (let* ((lim (make-test-limiter))
         (res (verify-login lim nil nil (lambda () :ok))))
    (is (eql :ok (compute-result-underlying-result res)))))

(test rate-limiting-then-ban
  "Tests rate limiting after max attempts per minute then banning"
  (let* ((lim (make-instance 'login-rate-limiter :constant-runtime 0.05 :max-attempts-per-minute 2 :computation (lambda () :ok)))
         (ip "ip"))
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim nil ip))))
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim nil ip))))

    (is (eql :rate-limited (compute-result-final-status
                             (verify-login lim nil ip))))
    (is (eql :banned-ip (compute-result-final-status
                          (verify-login lim nil ip))))))

(defun rand-str ()
  (ironclad:byte-array-to-hex-string (ironclad:random-data 8)))

(test user-limit-locked
  "Tests rate limiting and locking against a user id with random ips"
  (let* ((lim (make-login-rate-limiter :constant-runtime 0.05 :max-user-failures-per-minute 2 :computation (lambda () nil)))
         (user-id "user"))
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim user-id (rand-str)))))
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim user-id (rand-str)))))
    ;; since the failure isn't recorded until after a computation, the max technically allows for 3 fails instead of 2 like the ip ban
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim user-id (rand-str)))))

    (is (eql :locked-user (compute-result-final-status
                            (verify-login lim user-id (rand-str)))))
    (is (eql :locked-user (compute-result-final-status
                            (verify-login lim user-id (rand-str)))))))

(test ip-unbanned
  "Tests ip gets unbanned after being banned, but still rate limited again"
  (let* ((lim (make-login-rate-limiter :constant-runtime 0.05 :max-attempts-per-minute 1 :computation (lambda () :ok)
                                       :ban-duration-minutes (/ 0.2 60)
                                       :cleanup-interval-minutes (/ 0.1 60)))
         (ip "1234"))
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim nil ip))))
    (is (eql :rate-limited (compute-result-final-status
                             (verify-login lim nil ip))))
    (is (eql :banned-ip (compute-result-final-status
                          (verify-login lim nil ip))))

    (sleep 0.25)
    ;; should still be rate limited next (and then re-banned)
    ;; because while after sleeping the ban has expired, we're still
    ;; exceeding the requests per minute threshold
    (is (eql :rate-limited (compute-result-final-status
                             (verify-login lim nil ip))))))

(test user-unlocked
  "Tests user id gets locked but then unlocked"
  (let* ((lim (make-login-rate-limiter :constant-runtime 0.05 :max-user-failures-per-minute 1 :computation (lambda () nil)
                                       :lock-user-duration-minutes (/ 0.2 60)
                                       :cleanup-interval-minutes (/ 0.1 60)))
         (user-id "user"))
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim user-id (rand-str)))))
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim user-id (rand-str)))))
    (is (eql :locked-user (compute-result-final-status
                            (verify-login lim user-id (rand-str)))))

    (sleep 0.25)
    (is (eql :succeeded (compute-result-final-status
                          (verify-login lim user-id (rand-str)))))))

#+sbcl
(test cleanup
  "Just a short sbcl GC as an attempt to clean up any threads created by above. Keeps the webdriver vom log outputs cleaner too."
  (sb-ext:gc :full t)
  (sleep 0.2))

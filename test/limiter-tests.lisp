(defpackage #:anansi-tests
  (:use #:cl #:fiveam #:com.thejach.anansi))
(in-package #:anansi-tests)
(import 'com.thejach.anansi::now-seconds)

(setf *run-test-when-defined* t)

(def-suite limiter-tests)

(in-suite limiter-tests)

(defun dummy-fast-computation ()
  :ok)

(test limiter-basic-success
  (let* ((lim (make-limiter :computation #'dummy-fast-computation
                            :constant-runtime 0.2
                            :jitter 0.0
                            :concurrency 1
                            :max-wait 0.1))
         (start (now-seconds))
         (result (compute lim))
         (elapsed (- (now-seconds) start)))
    (format t "res ~A~%" result)
    (is-true (compute-result-underlying-finished? result))
    (is (eql :ok (compute-result-underlying-result result)))
    (is (eql :succeeded (compute-result-final-status result)))
    (is-true (<= 0.18 elapsed 0.25) "Computation did not respect constant-runtime timing")))

(test limiter-exceed-max-wait
  "Force a wait longer than max-wait to trigger :exceeded-max-wait."
  (let* ((lim (make-limiter :computation (lambda () (sleep 0.3) :slow)
                            :constant-runtime 0.5
                            :jitter 0.0
                            :concurrency 1
                            :max-wait 0.05)))
    ;; Occupy the only slot so next compute must wait.
    (bt:make-thread (lambda () (compute lim)))
    (sleep 0.01) ; ensure other thread has taken the semaphore
    (let ((result (compute lim)))
      (is-false (compute-result-underlying-finished? result))
      (is (eql :exceeded-max-wait (compute-result-final-status result))))))

(test limiter-hard-backpressure
  "Force the limiter's wait semaphore to be initialized and exhausted."
  (let* ((lim (make-limiter :computation (lambda () (sleep 0.1))
                            :constant-runtime 0.3
                            :jitter 0.0
                            :concurrency 1
                            :max-wait 0.2)))
    (compute lim)
    ;; Manually exhaust the wait semaphore
    (dotimes (_ 8)
      (bt:make-thread (lambda () (compute lim))))
    (let ((result (compute lim)))
      (is-false (compute-result-underlying-finished? result))
      (is (eql :wait-limit-full (compute-result-final-status result))))))

(test limiter-jitter
  "Check that jitter makes elapsed >= constant-runtime"
  (let* ((lim (make-limiter :computation #'dummy-fast-computation
                            :constant-runtime 0.1
                            :jitter 0.2
                            :concurrency 1
                            :max-wait 0.05))
         (start (now-seconds))
         (result (compute lim))
         (elapsed (- (now-seconds) start)))
    (is (eql :succeeded (compute-result-final-status result)))
    (is-true (>= elapsed 0.1) (format nil "elapsed must be at least constant-runtime, was ~a" elapsed))
    (is-true (<= elapsed 0.35) "elapsed must not exceed constant-runtime+jitter by much")))




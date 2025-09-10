(defpackage #:anansi-tests
  (:use #:cl #:fiveam #:com.thejach.anansi))
(in-package #:anansi-tests)
(shadowing-import 'com.thejach.anansi::now-seconds)

;(setf *run-test-when-defined* t)

(def-suite limiter-tests)

(in-suite limiter-tests)

(defun dummy-fast-computation ()
  :ok)

(defun dummy-slow-computation (&optional (duration 0.2) spin-loop?)
  (if spin-loop?
      (let ((start (now-seconds)))
        (loop until (>= (- (now-seconds) start) duration)))
      (sleep duration))
  :fake-bcrypt)

(defun eps-eql? (expected actual &optional (epsilon 0.01))
  "Tests if actual float value is within epsilon of expected"
  (<= (- expected epsilon) actual (+ expected epsilon)))

(test limiter-basic-success
  "Basic test that a computation can run"
  (let* ((lim (make-limiter :computation #'dummy-fast-computation
                            :constant-runtime 0.2
                            :jitter 0.0
                            :concurrency 1
                            :max-wait 0.1))
         (start (now-seconds))
         (result (compute lim))
         (elapsed (- (now-seconds) start)))
    (is-true (compute-result-underlying-finished? result))
    (is (eql :ok (compute-result-underlying-result result)))
    (is (eql :succeeded (compute-result-final-status result)))
    (is (eps-eql? 0.2 elapsed) "Computation did not respect constant-runtime timing, was ~,2fs" elapsed)))

(test limiter-exceed-max-wait
  "Force a wait longer than max-wait to trigger :exceeded-max-wait."
  (let* ((lim (make-limiter :computation #'dummy-slow-computation
                            :constant-runtime 0.5
                            :jitter 0.0
                            :concurrency 1
                            :max-wait 0.05)))
    ;; Occupy the only slot so next compute must wait.
    (bt:make-thread (lambda () (compute lim)))
    (sleep 0.01) ; ensure other thread has taken the semaphore
    (let ((result (compute lim)))
      (is-false (compute-result-underlying-finished? result))
      (is (eql nil (compute-result-underlying-result result)))
      (is (eql :exceeded-max-wait (compute-result-final-status result))))))

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
    (is-true (>= elapsed 0.1) "elapsed must be at least constant-runtime, was ~,2fs" elapsed)
    (is-true (<= elapsed 0.35) "elapsed must not exceed constant-runtime+jitter by much, was ~,2fs" elapsed)))

(test limiter-hard-backpressure
  "Force the limiter's wait semaphore to be initialized and exhausted."
  (let* ((lim (make-limiter :computation #'dummy-slow-computation
                            :constant-runtime 0.8
                            :jitter 0.0
                            :concurrency 2
                            :max-wait 0.41)))
    (compute lim)
    ; first run observes compute runtime b ~= 0.2, sets q-max to:
    ; max(1, floor(k * w/b) - 1)
    ; = (max 1 (1- (floor (/ (* 2 0.41) 0.2))))
    ; = 3
    (is (eql 3 (com.thejach.anansi::.q-max lim)))
    ; thus max-in-flight should be 3+2 (q-max + k) with subsequent attempts hitting the wait-limit-full status
    (dotimes (_ 5)
      (bt:make-thread (lambda () (compute lim))))
    (sleep 0.01)
    (let ((result (compute lim)))
      (is-false (compute-result-underlying-finished? result))
      (is (eql :wait-limit-full (compute-result-final-status result))))))

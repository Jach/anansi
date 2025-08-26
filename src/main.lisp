(in-package #:com.thejach.anansi)

(defun now-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun sleep-until (target-time-seconds)
  "Given a target-time-seconds that represents a time in the future,
   determine how many seconds away from now that is, and if positive,
   sleep until that time is reached."
  (let ((remaining-seconds (- target-time-seconds (now-seconds))))
    (when (plusp remaining-seconds)
      (sleep remaining-seconds))))

#+nil
(let ((5s-from-now (+ 5 (now-seconds))))
  (time (sleep-until 5s-from-now)))

(defclass limiter ()
  ((computation :accessor .computation
                :initarg :computation
                :initform (lambda ())
                :documentation "A provided function taking no arguments that will be run under the limiter. May or may not be called when COMPUTE is called. Can be changed with the accessor,
                                or even left unspecified. COMPUTE takes an optional function override that will ignore this value.")
   (constant-runtime :accessor .constant-runtime
                     :initarg :constant-runtime
                     :initform 1.0
                     :documentation "Value in seconds for which computations with this limiter will try to always execute in, regardless of whether the underlying computation finished earlier.")
   (jitter :accessor .jitter
           :initarg :jitter
           :initform 0.1
           :documentation "Value in seconds for which computations add a random amount of time up to jitter on top of the computation duration to further lessen any timing attack leakage.")
   (concurrency :accessor .concurrency
                :initarg :concurrency
                :initform 4
                :documentation "The number of concurrent computations that can happen with this limiter. Should probably be at or below the machine core count. Affects how many additional computations can be waiting.")
   (max-wait :accessor .max-wait
             :initarg :max-wait
             :initform 0.6
             :documentation "The maximum time in seconds, that should be less than constant-runtime, that a computation is allowed to wait before starting. Affects how many additional computations can be waiting.")

   (%compute-sema :accessor .compute-sema
                  :documentation "Internal semaphore gating computation access. Sized to concurrency. Allows for that many computations to proceed in parallel while others must wait to start.")
   (%q-max :accessor .q-max
           :initform 1
           :documentation "Internal estimate of Q_max, or how many computations are allowed to begin waiting for their turn, a kind of 'queue'. Its value is calculated and set after the first observed timing of a computation.
                           If the computation happens so fast that the first measured time is 0, we set an arbitrary minimum estimate of typical computation times to be 20% of the constant-runtime. This will also
                           always be at least 1.")
   (%wait-access-sema :accessor .wait-access-sema
                      :initform nil
                      :documentation "Internal semaphore gating wait access. Sized to %q-max once known. Provides hard back-pressure such that a flood of requests will jump immediately to waiting for constant-runtime instead of
                                      contending for the computation semaphore or performing the underlying computation.")))

(defmethod initialize-instance :after ((limiter limiter) &key)
  (setf (.compute-sema limiter) (bt:make-semaphore :count (.concurrency limiter))))

(defun make-limiter (&key (computation (lambda ())) constant-runtime jitter concurrency max-wait)
  (make-instance 'limiter
                 :computation computation
                 :constant-runtime constant-runtime
                 :jitter jitter
                 :concurrency concurrency
                 :max-wait max-wait))

(defstruct compute-result
  "The result of a compute call so callers can know what happened and decide what to expose. final-status will be :succeeded and is redundant if underlying-finished? is true,
   but if underlying-finished? is false then final-status will be a more descriptive keyword such as :exceeded-max-wait or :wait-limit-full."
  underlying-finished?
  underlying-result
  final-status)

(defmacro with-computation ((limiter) &body body)
  "Uses BODY as the computation to potentially execute within the context of the limiter's compute call."
  `(compute ,limiter (lambda () ,@body)))

(defmethod compute ((limiter limiter) &optional override-computation)
  "Runs the limiter's computation in constant-runtime + jitter time no matter what.
   Returns a compute-result struct."
  (let* ((time-now (now-seconds))
         (target-duration (+ time-now (.constant-runtime limiter) (* (random 1.0) (.jitter limiter))))
         (wait-access-sema (.wait-access-sema limiter)))
    (if (and wait-access-sema ; sema is ready
             (not (bt:wait-on-semaphore wait-access-sema :timeout least-positive-double-float)))
        (progn ; Hard back-pressure, skip any attempt at computing or waiting to compute
          (sleep-until target-duration)
          (make-compute-result :underlying-finished? nil :underlying-result nil :final-status :wait-limit-full))

        (if (bt:wait-on-semaphore (.compute-sema limiter) :timeout (.max-wait limiter))
            (let ((result nil)) ; proceed and do computation
              (when wait-access-sema
                (bt:signal-semaphore wait-access-sema)) ; allow others to join the waiting
              (unwind-protect
                (setf result
                      (funcall (if override-computation
                                   override-computation
                                   (.computation limiter))))

                (let ((wait-and-computation-time (- (now-seconds) time-now)))
                  (bt:signal-semaphore (.compute-sema limiter))
                  (when (not wait-access-sema) ; first run
                    (prepare-wait-semaphore limiter wait-and-computation-time))
                  (sleep-until target-duration)))
              (make-compute-result :underlying-finished? t :underlying-result result :final-status :succeeded))

            (progn
              (sleep-until target-duration)
              (make-compute-result :underlying-finished? nil :underlying-result nil :final-status :exceeded-max-wait))))))

(defun prepare-wait-semaphore (limiter wait-and-computation-time)
  (let ((q-max (max 1 (1- (floor
                            (* (.concurrency limiter)
                               (/ (.max-wait limiter) (if (zerop wait-and-computation-time)
                                                          (* 0.2 (.constant-runtime limiter))
                                                          wait-and-computation-time))))))))
    (setf (.q-max limiter) q-max)
    (setf (.wait-access-sema limiter) (bt:make-semaphore :count q-max))))


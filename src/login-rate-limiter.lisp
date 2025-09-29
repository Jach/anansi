(in-package #:com.thejach.anansi)

(defclass login-rate-limiter (limiter)
  ((max-attempts-per-minute :accessor .max-attempts-per-minute
                            :initarg :max-attempts-per-minute
                            :initform 6
                            :documentation "Max login tries per minute a single IP can make on any account before they are banned from further attempts.")
   (ban-duration-minutes :accessor .ban-duration-minutes
                         :initarg :ban-duration-minutes
                         :initform 30
                         :documentation "How many minutes a single IP is banned for where any attempt to login to any user will fail.")

   (max-user-failures-per-minute :accessor .max-user-failures-per-minute
                                 :initarg :max-user-failures-per-minute
                                 :initform 6
                                 :documentation "Max login failures per minute that a single user can have made on their account before it is locked from further attempts.")
   (lock-user-duration-minutes :accessor .lock-user-duration-minutes
                               :initarg :lock-user-duration-minutes
                               :initform 10
                               :documentation "How many minutes a user is locked, where any attempts to login to that user from any IP will fail.")

   (cleanup-interval-minutes :accessor .cleanup-interval-minutes
                             :initarg :cleanup-interval-minutes
                             :initform 15
                             :documentation "How often a maintenance thread runs to clean up records and do any necessary unbanning/unlocking.")

   (%ip-attempts :accessor .ip-attempts
                 :initform (cht:make-castable :test #'equal)
                 :documentation "Map of IP -> list of attempt timestamps.")
   (%banned-ips :accessor .banned-ips
                :initform (cht:make-castable :test #'equal)
                :documentation "Map of IP -> ban expiry timestamp.")

   (%user-failures :accessor .user-failures
                   :initform (cht:make-castable :test #'equal)
                   :documentation "Map of user key -> list of failure timestamps.")
   (%locked-users :accessor .locked-users
                  :initform (cht:make-castable :test #'equal)
                  :documentation "Map of user key -> locked out expiry timestamp.")

   (%purge-records-after-minutes :accessor .purge-records-after-minutes
                                 :initform 60
                                 :documentation "How many minutes old observations can be before they're purged.")
   (%maintenance-thread :accessor .maintenance-thread
                        :initform nil
                        :documentation "Thread for maintenance tasks."))

  (:documentation "A variant of the main limiter class. This version exposes a new verify-login method to aid in limiting
                   attempts to an underlying expensive pw hash computation by IP address and also on a user id/key."))

(defmethod initialize-instance :after ((self login-rate-limiter) &key)
  (log self :debug (format nil "Created new LOGIN-RATE-LIMITER object with options: ~a" (list :max-attempts-per-minute (.max-attempts-per-minute self)
                                                                                              :ban-duration-minutes (.ban-duration-minutes self)
                                                                                              :max-user-failures-per-minute (.max-user-failures-per-minute self)
                                                                                              :lock-user-duration-minutes (.lock-user-duration-minutes self)
                                                                                              :cleanup-interval-minutes (.cleanup-interval-minutes self))))
  (start-maintenance-thread self)
  (let ((prometheus:*default-registry* (.registry self))
        (tbl (.stored-metrics self)))
    (setf (gethash :login-rate-limiter-banned-ips tbl) (prometheus:make-counter :name "login_rate_limiter_banned_ips_total"
                                                                                :help "Total count of the limiter's banned IPs.")
          (gethash :login-rate-limiter-unbanned-ips tbl) (prometheus:make-counter :name "login_rate_limiter_unbanned_ips_total"
                                                                                  :help "Total count of the limiter's unbanned IPs.")

          (gethash :login-rate-limiter-locked-users tbl) (prometheus:make-counter :name "login_rate_limiter_locked_users_total"
                                                                                  :help "Total count of the limiter's locked users.")
          (gethash :login-rate-limiter-unlocked-users tbl) (prometheus:make-counter :name "login_rate_limiter_unlocked_users_total"
                                                                                    :help "Total count of the limiter's unlocked users.")
          (gethash :login-rate-limiter-verify-successes tbl) (prometheus:make-counter :name "login_rate_limiter_verify_successes"
                                                                                      :help "Total count of VERIFY-LOGIN running and returning a true result.")
          (gethash :login-rate-limiter-verify-failures tbl) (prometheus:make-counter :name "login_rate_limiter_verify_failures"
                                                                                     :help "Total count of VERIFY-LOGIN running and returning a nil result."))))


(defun make-login-rate-limiter (&key (max-attempts-per-minute 6) (ban-duration-minutes 30) (max-user-failures-per-minute 6) (lock-user-duration-minutes 10) (cleanup-interval-minutes 15)
                                     (computation (lambda ())) (constant-runtime 1.0) (jitter 0.1) (concurrency 4) (max-wait 0.6))
  (make-instance 'login-rate-limiter
                 :max-attempts-per-minute max-attempts-per-minute
                 :ban-duration-minutes ban-duration-minutes
                 :max-user-failures-per-minute max-user-failures-per-minute
                 :lock-user-duration-minutes lock-user-duration-minutes
                 :cleanup-interval-minutes cleanup-interval-minutes
                 :computation computation
                 :constant-runtime constant-runtime
                 :jitter jitter
                 :concurrency concurrency
                 :max-wait max-wait))

(defmethod verify-login ((self login-rate-limiter) user-key ip &optional override-computation drop-immediately?)
  "Possibly runs the underlying computation subject to the login rate limiting policies.
   The underlying computation (overridable with OVERRIDE-COMPUTATION) should return a true value if the result is 'verified',
   and nil otherwise. When given a USER-KEY parameter, a nil result is used to mark the try as a failure, with too many resulting
   in the user-key being locked for some amount of time and subsequent attempts to verify resulting in an instant failure.

   This method uses the historical frequency of verify-login attempts on a user-key and/or from an IP address,
   as well as the historical success or failure of the underlying computation,
   to determinme whether to proceed with the actual underlying computation this time.

   If either user-key or ip are nil, their checks behave as if being skipped. If both are nil, this is functionally no different
   to calling COMPUTE.

   If it is decided to proceed, the returned result will be that of calling COMPUTE directly. Note the underlying computation can still fail to run,
   but not for reaosns related to the policies of the login rate limiter.

   If it is decided not to proceed, the returned result will be a failure with a reason in the status field such as :banned-ip.
   Unless drop-immediately? is given, the call duration will still be the configured constant duration."
  (when (banned-ip? self ip)
    (log self :info (format nil "Rejected login attempt from banned IP: ~a" ip))
    (return-from verify-login (make-login-failure self :banned-ip override-computation drop-immediately?)))

  (record-ip-attempt self ip)
  (when (rate-limit-reached? self ip)
    (ban-ip self ip)
    (return-from verify-login (make-login-failure self :rate-limited override-computation drop-immediately?)))

  (when (locked-user? self user-key)
    (log self :info (format nil "Rejected login attempt from IP ~a because user-key \"~a\" is locked" ip user-key))
    (return-from verify-login (make-login-failure self :locked-user override-computation drop-immediately?)))

  (let ((res (compute self override-computation)))
    (when (compute-result-underlying-finished? res)
      (if (compute-result-underlying-result res)
          (mark-success self user-key ip)
          (mark-failure self user-key ip))
    res)))

(defun mark-success (self user-key ip)
  (log self :info (format nil "Successful login-rate-limiter computation from IP ~a against user-key \"~a\"" ip user-key))
  (prometheus:counter.inc (gethash :login-rate-limiter-verify-successes (.stored-metrics self))))

(defun mark-failure (self user-key ip)
  (log self :info (format nil "Failed login-rate-limiter computation from IP ~a against user-key \"~a\"" ip user-key))
  (prometheus:counter.inc (gethash :login-rate-limiter-verify-failures (.stored-metrics self)))
  (record-user-failure self user-key)
  (when (user-rate-reached? self user-key)
    (lock-user self user-key)))

(defmethod make-login-failure ((self login-rate-limiter) reason override-computation drop-immediately?)
  (if drop-immediately?
      (make-compute-result :underlying-finished? nil :underlying-result nil :final-status reason)
      (let ((res (compute self override-computation t)))
        (setf (compute-result-final-status res) reason
              (compute-result-underlying-finished? res) nil)
        res)))


(defun expiry-present? (table key)
  (let ((expiry (cht:gethash key table)))
    (and expiry (> expiry (now-seconds)))))

(defmethod banned-ip? ((self login-rate-limiter) ip)
  (and ip (expiry-present? (.banned-ips self) ip)))

(defmethod locked-user? ((self login-rate-limiter) user-key)
  (and user-key (expiry-present? (.locked-users self) user-key)))


(defun record-attempt (table key)
  (let ((attempts (cht:gethash key table)))
    (if attempts
        (push (now-seconds) (cht:gethash key table))
        (setf (cht:gethash key table) (list (now-seconds))))))

(defmethod record-ip-attempt ((self login-rate-limiter) ip)
  (when ip
    (record-attempt (.ip-attempts self) ip)))

(defmethod record-user-failure ((self login-rate-limiter) user-key)
  (when user-key
    (record-attempt (.user-failures self) user-key)))


(defun max-rate-reached? (table key max-per-minute)
  (let* ((one-minute-ago (- (now-seconds) 60))
         (recent-attempts (count-if (lambda (timestamp) (> timestamp one-minute-ago))
                                    (cht:gethash key table))))
    (> recent-attempts max-per-minute)))

(defmethod rate-limit-reached? ((self login-rate-limiter) ip)
  (and ip (max-rate-reached? (.ip-attempts self) ip (.max-attempts-per-minute self))))

(defmethod user-rate-reached? ((self login-rate-limiter) user-key)
  (and user-key (max-rate-reached? (.user-failures self) user-key (.max-user-failures-per-minute self))))


(defun set-expiry (table key duration)
  (let ((expiry (+ (now-seconds) (* 60 duration))))
    (setf (cht:gethash key table) expiry)))

(defmethod ban-ip ((self login-rate-limiter) ip)
  (when ip
    (log self :info (format nil "Rate limit reached, banning IP: ~a" ip))
    (prometheus:counter.inc (gethash :login-rate-limiter-banned-ips (.stored-metrics self)))
    (set-expiry (.banned-ips self) ip (.ban-duration-minutes self))))

(defmethod lock-user ((self login-rate-limiter) user-key)
  (when user-key
    (log self :info (format nil "Max attempts on user-key \"~a\" reached, locking user" user-key))
    (prometheus:counter.inc (gethash :login-rate-limiter-locked-users (.stored-metrics self)))
    (set-expiry (.locked-users self) user-key (.lock-user-duration-minutes self))))

(defmethod start-maintenance-thread ((self login-rate-limiter))
  "Starts and stores the background maintenance thread.
   The use of a weak pointer should allow the thread to stop on its own
   if the parent limiter is GC'd."
  (let ((weak-ptr (trivial-garbage:make-weak-pointer self)))
    (let ((thread (bt:make-thread (lambda ()
                                    (loop
                                      (unless (cleanup weak-ptr)
                                        (return))))
                                  :name (format nil "login-rate-limiter-maintenance-thread-~,1f" (now-seconds)))))
      (setf (.maintenance-thread self) thread))))

(defun cleanup (weak-ptr)
  "Does the actual cleanup tasks, sleeping for the configured interval between runs.
   Note: these used to be generic functions on the weak ref's value, but that seemed to
   prevent GC and sb-ext:search-roots would find a path."
  (cleanup-old-attempts weak-ptr)
  (cleanup-expired-bans weak-ptr)
  (cleanup-locked-users weak-ptr)
  (alexandria:when-let ((limiter (trivial-garbage:weak-pointer-value weak-ptr)))
    (log (format nil "~a" limiter) :debug (format nil "Periodic cleanup completed. Next cleanup in ~a minutes. Counts of tables: (~@{~a ~})"
                                                  (.cleanup-interval-minutes limiter)
                                                  :ip-attempts (cht:count (.ip-attempts limiter))
                                                  :user-failures (cht:count (.user-failures limiter))
                                                  :banned-ips (cht:count (.banned-ips limiter))
                                                  :locked-users (cht:count (.locked-users limiter)))))
  (alexandria:when-let ((limiter (trivial-garbage:weak-pointer-value weak-ptr)))
    (sleep (* 60 (.cleanup-interval-minutes limiter)))
    t))

(defun purge (table purge-limit)
  (cht:maphash (lambda (key attempts)
                 (when (listp attempts)
                   (let ((filtered-attempts (remove-if (lambda (timestamp) (< timestamp purge-limit))
                                                       attempts)))
                     (if filtered-attempts
                         (setf (cht:gethash key table) filtered-attempts)
                         (cht:remhash key table)))))
               table))

(defun cleanup-old-attempts (limiter-ptr)
  "Keeps the hash tables from growing too much by removing any records whose last timestamp was older than the purge time."
  (alexandria:when-let ((limiter (trivial-garbage:weak-pointer-value limiter-ptr)))
    (let ((purge-limit (- (now-seconds) (* 60 (.purge-records-after-minutes limiter)))))
      (purge (.ip-attempts limiter) purge-limit)
      (purge (.user-failures limiter) purge-limit))))

(defun remove-expiry (table counter)
  (let ((now (now-seconds))
        (removed-keys (list)))
    (cht:maphash (lambda (key expiry)
                   (when (and (numberp expiry) ;; seems expiry can sometimes be a luckless.hashtable::tombstone. so much for this cht...
                              (< expiry now))
                     (prometheus:counter.inc counter)
                     (push key removed-keys)
                     (cht:remhash key table)))
                 table)
    removed-keys))

(defun cleanup-expired-bans (limiter-ptr)
  "Unbans any IPs whose ban duration has passed."
  (alexandria:when-let ((limiter (trivial-garbage:weak-pointer-value limiter-ptr)))
    (alexandria:when-let ((removed-keys (remove-expiry (.banned-ips limiter) (gethash :login-rate-limiter-unbanned-ips (.stored-metrics limiter)))))
      (log (format nil "~a" limiter) :info (format nil "Unbanned IPs ~a" removed-keys)))))

(defun cleanup-locked-users (limiter-ptr)
  "Unlocks any users whose lock duration has passed."
  (alexandria:when-let ((limiter (trivial-garbage:weak-pointer-value limiter-ptr)))
    (alexandria:when-let ((removed-keys (remove-expiry (.locked-users limiter) (gethash :login-rate-limiter-unlocked-users (.stored-metrics limiter)))))
      (log (format nil "~a" limiter) :info (format nil "Unlocked user-keys ~a" removed-keys)))))


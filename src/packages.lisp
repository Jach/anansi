(defpackage #:com.thejach.anansi
  (:use #:cl)
  (:shadow #:log)
  (:local-nicknames (#:cht #:org.shirakumo.luckless.hashtable))
  (:export #:limiter
           #:make-limiter

           #:.computation
           #:compute
           #:with-computation

           #:compute-result-underlying-finished?
           #:compute-result-underlying-result
           #:compute-result-final-status
           #:compute-result-entered-at
           #:compute-result-exited-at

           #:login-rate-limiter
           #:make-login-rate-limiter
           #:verify-login

           #:.registry
           #:.stored-metrics

           #:*logger*))



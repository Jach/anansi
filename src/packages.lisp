(defpackage #:com.thejach.anansi
  (:use #:cl)
  (:local-nicknames (#:cht #:org.shirakumo.luckless.hashtable))
  (:export #:limiter
           #:make-limiter

           #:.computation
           #:compute
           #:with-computation

           #:compute-result-underlying-finished?
           #:compute-result-underlying-result
           #:compute-result-final-status

           #:login-rate-limiter
           #:verify-login))



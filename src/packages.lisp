(defpackage #:com.thejach.anansi
  (:use #:cl)
  (:export #:limiter
           #:make-limiter

           #:.computation
           #:compute
           #:with-computation

           #:compute-result
           #:compute-result-underlying-finished?
           #:compute-result-underlying-result
           #:compute-result-final-status))



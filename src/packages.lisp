(defpackage #:com.thejach.anansi
  (:use #:cl)
  (:export #:limiter
           #:make-limiter

           #:compute

           #:compute-result
           #:compute-result-underlying-finished?
           #:compute-result-underlying-result
           #:compute-result-final-status))



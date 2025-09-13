(defpackage #:com.thejach.anansi/example.config
  (:use #:cl)
  (:export #:app-root
           #:config
           #:start
           #:acceptor))

(in-package #:com.thejach.anansi/example.config)

(defparameter *common-config* '(:server-port 56142))

(defun app-root ()
  (merge-pathnames #p"example/" (asdf:system-source-directory "anansi/example")))

(defun config (&optional key)
  "Wrapper function to pull out named configuration values"
  (getf *common-config* key))

(defun start ()
  "Should be called when the server starts, can set up any runtime or per-server stuff here."
  (setf (getf *common-config* :db-name) (merge-pathnames #P"db.sqlite3" (app-root))
        (getf *common-config* :static-dir) (merge-pathnames #P"static/" (app-root))))

(defclass acceptor (easy-routes:routes-acceptor)
  ())

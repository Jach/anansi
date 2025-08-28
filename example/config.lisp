(defpackage #:com.thejach.anansi/example.config
  (:use #:cl)
  (:export #:app-root
           #:static-dir
           #:config
           #:acceptor))

(in-package #:com.thejach.anansi/example.config)

(defun app-root ()
  (merge-pathnames #p"example/" (asdf:system-source-directory "anansi/example")))

(defun static-dir ()
  (merge-pathnames #p"static/" (app-root)))

(defparameter *common-config* '(:server-port 56142))

(defun config (&optional key)
  (getf *common-config* key))

(defclass acceptor (easy-routes:routes-acceptor)
  ())

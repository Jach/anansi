(defpackage #:com.thejach.anansi/example.config
  (:use #:cl)
  (:export #:app-root
           #:server-port
           #:db-name
           #:static-dir
           #:acceptor))

(in-package #:com.thejach.anansi/example.config)

(defun app-root ()
  (merge-pathnames #p"example/" (asdf:system-source-directory "anansi/example")))

(defun server-port ()
  56142)

(defun db-name ()
  (merge-pathnames #P"db.sqlite3" (app-root)))

(defun static-dir ()
  (merge-pathnames #P"static/" (app-root)))

(defclass acceptor (easy-routes:routes-acceptor)
  ())

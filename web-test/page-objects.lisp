(in-package #:anansi-web-tests)

;;;; This file is to support selenium webdriver tests. Details about URLs and where on the page various elements are should be
;;;; encapsulated in here.
;;;; We should probably make a home page and logged-in page object but for now we just make 'login' and 'register' page objects (even though they're
;;;; both on the home page).

(defun base-url ()
  (format nil "http://localhost:~d/" (com.thejach.anansi/example.config:server-port)))

(defun welcome-url ()
  (format nil "~Awelcome" (base-url)))

(defun on-home-page? ()
  (webdriver-client-utils:wait-for "#greetings")
  (equal (base-url) (webdriver:url)))

(defun navigate-home ()
  (setf (webdriver:url) (base-url))
  (on-home-page?))

(defun on-welcome-page? ()
  (webdriver-client-utils:wait-for "#logout-link")
  (equal (welcome-url) (webdriver:url)))

(defun logout ()
  (webdriver:element-click (webdriver:find-element "#logout-link"))
  (on-home-page?))

(defclass registration-page ()
  ((username :accessor .username :initform "#register-username")
   (password :accessor .password :initform "#register-password")
   (password2 :accessor .password2 :initform "#register-password2")
   (submit :accessor .submit :initform "#register-submit")))

(defmethod enter-username (page username)
  (webdriver:element-send-keys (webdriver:find-element (.username page)) username))

(defmethod enter-password (page password)
  (webdriver:element-send-keys (webdriver:find-element (.password page)) password))

(defmethod enter-password2 ((page registration-page) password)
  (webdriver:element-send-keys (webdriver:find-element (.password2 page)) password))

(defmethod click-submit-button (page)
  (webdriver:element-click (webdriver:find-element (.submit page))))

(defclass login-page ()
  ((username :accessor .username :initform "#login-username")
   (password :accessor .password :initform "#login-password")
   (submit :accessor .submit :initform "#login-submit")))

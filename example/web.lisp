(defpackage #:com.thejach.anansi/example.web
  (:use #:cl #:easy-routes)
  (:local-nicknames (#:config #:com.thejach.anansi/example.config))
  (:export #:start
           #:stop))

(in-package #:com.thejach.anansi/example.web)

;;; Server

(defvar *server* nil)

(defun start (port)
  (setf *server* (make-instance 'config:acceptor
                                :port port
                                :name 'anansi-web))
  (hunchentoot:start *server*))

(defun stop ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))

;;; HTML Components

(defun csrf ()
  "Insert a hidden CSRF token input."
  (let ((token (generate-csrf-token)))
    (setf (hunchentoot:session-value :csrf-token) token)
    (spinneret:with-html
      (:input :type "hidden" :name "csrf-token" :value token))))

(defun generate-csrf-token ()
  "Return a random CSRF token."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(defun csrf-valid? (request)
  (let ((submitted (hunchentoot:post-parameter "csrf-token" request))
        (session (hunchentoot:session-value :csrf-token)))
    (and submitted session (string= submitted session))))

(defun login-form (&optional error-msg)
  (spinneret:with-html
    (:div :class "card login"
      (:h2 "Login")
      (:form :hx-post "/login" :hx-swap "outerHTML" :hx-target "closest div" :hx-indicator ".spinner"
        (:div :class "form-group"
          (:label :for "username" "Username")
          (:input :type "text" :id "username" :name "username" :required ""))
        (:div :class "form-group"
          (:label :for "password" "Password")
          (:input :type "password" :id "password" :name "password" :required ""))
        (csrf)
        (:button :type "submit" "Login")
        (:div :class "spinner hidden"))
      (when error-msg
        (:div :class "error-msg" error-msg)))))

(defun register-form (&optional error-msg)
  (spinneret:with-html
    (:div :class "card register"
      (:h2 "Register")
      (:form :hx-post "/register" :hx-swap "outerHTML" :hx-target "closest div" :hx-indicator ".spinner"
        (:div :class "form-group"
          (:label :for "username" "Username")
          (:input :type "text" :id "username" :name "username" :required ""))
        (:div :class "form-group"
          (:label :for "password" "Password")
          (:input :type "password" :id "password" :name "password" :required ""))
        (:div :class "form-group"
          (:label :for "password2" "Re-enter Password")
          (:input :type "password" :id "password2" :name "password2" :required ""))
        (csrf)
        (:button :type "submit" "Register")
        (:div :class "spinner hidden"))
      (when error-msg
        (:div :class "error-msg" error-msg)))))

;;; Route decorators

(defun @csp (next)
  (setf (hunchentoot:header-out "Content-Security-Policy") "default-src 'self'; script-src 'self' 'unsafe-eval';")
  (setf (hunchentoot:header-out "X-Frame-Options") "SAMEORIGIN")
  (funcall next))

(defun @html (next)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (funcall next))

;;; Routes

(defroute home ("/" :acceptor-name anansi-web :decorators (@csp @html)) ()
  (spinneret:with-html-string
    (:doctype)
    (:html
      (:head
        (:title "Login and Registration test")
        (:link :rel "stylesheet" :type "text/css" :href "/style.css")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:meta :name "htmx-config" :content "{ \"includeIndicatorStyles\": false }"))
      (:body
        (:div :class "container"
          (:h1 "Welcome")
          (:div :class "forms"
            (login-form)
            (register-form)))
        (:script :type "text/javascript" :src "/htmx.min.js")))))

(defroute login ("/login" :method :post :acceptor-name anansi-web :decorators (@csp @html)) ()
  (spinneret:with-html-string
    (login-form "Not implemented")))

(defroute register ("/register" :method :post :acceptor-name anansi-web :decorators (@csp @html)) ()
  (spinneret:with-html-string
    (register-form "Not implemented")))

;;; Static routes (nit: hunchentoot's normal create-folder-dispatcher-and-handler trick wasn't working and I couldn't figure out why)

(defroute css ("/style.css" :acceptor-name anansi-web) ()
  (hunchentoot:handle-static-file (merge-pathnames "style.css" (config:static-dir))))

(defroute htmx ("/htmx.min.js" :acceptor-name anansi-web) ()
  (hunchentoot:handle-static-file (merge-pathnames "htmx.min.js" (config:static-dir))))


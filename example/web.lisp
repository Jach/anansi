(defpackage #:com.thejach.anansi/example.web
  (:use #:cl #:easy-routes)
  (:local-nicknames (#:config #:com.thejach.anansi/example.config)
                    (#:db #:com.thejach.anansi/example.db)
                    (#:auth #:com.thejach.anansi/example.authentication)
                    (#:anansi #:com.thejach.anansi))
  (:export #:start
           #:stop)
  (:import-from #:spinneret
                #:with-html-string))

(in-package #:com.thejach.anansi/example.web)

;;; Server

(defvar *server* nil)

(defun start (&aux port)
  (setf port (config:server-port))
  (format t "Starting http://localhost:~a -- port ~a and app-root ~a~%" port port (config:app-root))
  ;; Hunchentoot uses cl:random, which is pants on head dumb.
  (setf hunchentoot:*session-secret* (ironclad:byte-array-to-hex-string (ironclad:random-data 32)))
  (setf *server* (make-instance 'config:acceptor
                                :port port
                                :name 'anansi-web))
  (hunchentoot:start *server*))

(defun stop ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))

;;; HTML Components

(defun csrf (prefix)
  "Insert a hidden CSRF token input.
   Prefix is a string, used to distinguish csrf tokens on different forms present on the same page."
  (let ((token (generate-csrf-token)))
    (setf (hunchentoot:session-value (intern (uiop:strcat prefix "-CSRF-TOKEN") :keyword)) token)
    (spinneret:with-html
      (:input :type "hidden" :name "csrf-token" :value token))))

(defun generate-csrf-token ()
  "Return a random CSRF token."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(defun csrf-valid? (prefix)
  (let ((submitted (hunchentoot:post-parameter "csrf-token"))
        (session (hunchentoot:session-value (intern (uiop:strcat prefix "-CSRF-TOKEN") :keyword))))
    ;(format t "~a~%" (list submitted session))
    (and submitted session (equal submitted session))))

(defun login-form (&optional error-msg (username ""))
  (spinneret:with-html
    (:div :class "card login"
      (:h2 "Login")
      (:form :hx-post "/login" :hx-swap "outerHTML" :hx-target "closest div" :hx-indicator ".spinner"
        (:div :class "form-group"
          (:label :for "login-username" "Username")
          (:input :type "text" :id "login-username" :name "username" :required "true" :value username))
        (:div :class "form-group"
          (:label :for "login-password" "Password")
          (:input :type "password" :id "login-password" :name "password" :required "true"))
        (csrf "LOGIN")
        (:button :id "login-submit" :type "submit" "Login")
        (:div :class "spinner hidden"))
      (when error-msg
        (:div :class "error-msg" error-msg)))))

(defun register-form (&optional error-msg (username ""))
  (spinneret:with-html
    (:div :class "card register"
      (:h2 "Register")
      (:form :hx-post "/register" :hx-swap "outerHTML" :hx-target "closest div" :hx-indicator ".spinner"
        (:div :class "form-group"
          (:label :for "register-username" "Username")
          (:input :type "text" :id "register-username" :name "username" :required "true" :value username))
        (:div :class "form-group"
          (:label :for "register-password" "Password")
          (:input :type "password" :id "register-password" :name "password" :required "true"))
        (:div :class "form-group"
          (:label :for "register-password2" "Re-enter Password")
          (:input :type "password" :id "register-password2" :name "password2" :required "true"))
        (csrf "REGISTER")
        (:button :id "register-submit" :type "submit" "Register")
        (:div :class "spinner hidden"))
      (when error-msg
        (:div :class "error-msg" error-msg)))))

;;; Route decorators and other helpers

(defun @csp (next)
  (setf (hunchentoot:header-out "Content-Security-Policy") "default-src 'self'; script-src 'self' 'unsafe-eval';")
  (setf (hunchentoot:header-out "X-Frame-Options") "SAMEORIGIN")
  (funcall next))

(defun @html (next)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (funcall next))

(defun @plain-text (next)
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  (funcall next))

(defun @csrf (next prefix)
  (if (csrf-valid? prefix)
      (funcall next)
      (redir "/")))

(defun @auth (next)
  "Make sure the user is authorized to be on this page."
  (if (hunchentoot:session-value :user-id)
      (funcall next)
      (redir "/")))

(defun redir (url)
  "Redirect with HTMX's preferred header if using it, or a classic 302 otherwise."
  (if (equal (hunchentoot:header-in* "HX-Request") "true")
      (setf (hunchentoot:header-out "HX-Location") url)
      (hunchentoot:redirect url)))

;;; Session / cookie management

(defun grant-session (username)
  (let ((user-data (db:with-connection (conn) (db:select-id-password-hash conn username)))
        (session (hunchentoot:start-session)))
    (setf (hunchentoot:session-value :user-id session) (gethash "id" user-data))))


;;; Routes

;; Get

(defroute home ("/" :acceptor-name anansi-web :decorators (@csp @html)) ()
  (if (not (hunchentoot:session-value :user-id))
      (with-html-string
        (:doctype)
        (:html
          (:head
            (:title "Login and Registration test")
            (:link :rel "stylesheet" :type "text/css" :href "/style.css")
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
            (:meta :name "htmx-config" :content "{ \"includeIndicatorStyles\": false }"))
          (:body
            (:div :class "container"
             (:h1 :id "greetings" "Hello")
             (:div :class "forms"
              (login-form)
              (register-form)))
            (:script :type "text/javascript" :src "/htmx.min.js"))))
      (redir "/welcome")))

(defroute welcome ("/welcome" :acceptor-name anansi-web :decorators (@csp @html @auth)) ()
  (with-html-string
    (:doctype)
    (:html
      (:head
        (:title "Welcome"))
      (:body
        (:h1 "Welcome to your user page, user id: " (hunchentoot:session-value :user-id))
        (:p "If you would like to log out, "
         (:a :id "logout-link" :href "/logout" :hx-boost "true" "click here") ".")))))

(defroute logout ("/logout" :acceptor-name anansi-web :decorators (@csp @html)) ()
  (when (hunchentoot:session-value :user-id)
    (hunchentoot:delete-session-value :user-id))
  (redir "/"))

(defroute metrics ("/metrics" :acceptor-name anansi-web :decorators (@csp @plain-text)) ()
  (prometheus.formats.text:marshal (com.thejach.anansi:.registry auth::*limiter*)))


;; Post

(defroute login ("/login" :method :post :acceptor-name anansi-web :decorators (@csp @html (@csrf "LOGIN"))) (&post username password)
  (when (find-if #'str:empty? (list username password))
    (return-from login (with-html-string (login-form "Error: Please provide a username and password."))))

  (handler-case
    (let* ((user-data (db:with-connection (conn) (db:select-id-password-hash conn username)))
           (pw-hash (gethash "password_hash" user-data "")))
      (when (str:empty? pw-hash)
        (return-from login (with-html-string (login-form "Error: No username found. We shouldn't mention that though because it is better to NOT let an attacker know whether an attempt is on a valid account."))))

      (let ((result (auth:verify-login (gethash "id" user-data) password pw-hash (hunchentoot:real-remote-addr))))
        (when (not (anansi:compute-result-underlying-finished? result))
          (return-from login (with-html-string (login-form (format nil "Error: Could not perform hash computation for (best omitted) reason ~a."
                                                                   (anansi:compute-result-final-status result))
                                                           username))))

        (when (not (anansi:compute-result-underlying-result result))
          ;; Note: convince yourself that it's safe to send username back in the form here and previously without sanitizing / html escaping it.
          ;; (If you try adding raw html tags to the error string, they'll be escaped. If you try adding tags to the username,
          ;; they seem not to be, but if you try to escape the value field with a double quote, the double quote is escaped.)
          (return-from login (with-html-string (login-form "Error: Password did not match (probably shouldn't tell you that)." username))))

        ;; Success
        (grant-session username)
        (redir "/welcome")))

    (error (err) (with-html-string (login-form (format nil "Error: Encountered unexpected error, probably from the DB, was: ~a." err)
                                               username)))))


(defroute register ("/register" :method :post :acceptor-name anansi-web :decorators (@csp @html (@csrf "REGISTER"))) (&post username password password2)
  (when (find-if #'str:empty? (list username password password2))
    (return-from register (with-html-string (register-form "Error: Please provide all 3 inputs."))))

  (when (not (equal password password2))
    (return-from register (with-html-string (register-form "Error: The 2 given passwords do not match." username))))

  (handler-case
    (let* ((user-data (db:with-connection (conn) (db:select-id-password-hash conn username)))
           (already-exists? (gethash "id" user-data)))
      (when already-exists?
        (return-from register (with-html-string (register-form "Error: That username is taken (and we shouldn't necessarily disclose that)."))))

      (let ((result (auth:generate-hash password (hunchentoot:real-remote-addr))))
        (if (not (anansi:compute-result-underlying-finished? result))
            (with-html-string
              (register-form (format nil "Error: Could not perform hash computation for reason ~a (we probably shouldn't disclose that)."
                                     (anansi:compute-result-final-status result))
                             username))

            (let ((hashed-pw (anansi:compute-result-underlying-result result)))
              (db:with-connection (conn :writer? t)
                (db:insert-user conn username hashed-pw))
              (grant-session username)
              (redir "/welcome")))))

    (error (err) (with-html-string (register-form (format nil "Error: Could not register user, probably from the DB. We should probably not disclose but: ~a." err)
                                                  username)))))


;;; Static routes (nit: hunchentoot's normal create-folder-dispatcher-and-handler trick wasn't working and I couldn't figure out why, such behavior
;;; has made me decide to make future sites with ningle/jingle backed by not-hunchentoot)

(defroute css ("/style.css" :acceptor-name anansi-web) ()
  (hunchentoot:handle-static-file (merge-pathnames "style.css" (config:static-dir))))

(defroute htmx ("/htmx.min.js" :acceptor-name anansi-web) ()
  (hunchentoot:handle-static-file (merge-pathnames "htmx.min.js" (config:static-dir))))


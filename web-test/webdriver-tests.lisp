(in-package #:anansi-web-tests)

(def-suite webdriver-suite)

(in-suite webdriver-suite)

(def-fixture fresh-session-server-and-db ()
  "Creates a fresh webdriver session, example server, and db."
  (let ((db-name #p"/tmp/test-db.sqlite))"))
    (uiop:delete-file-if-exists db-name)
    (cl-mock:with-mocks ()
      (cl-mock:answer (com.thejach.anansi/example.config:server-port) 8182)
      (cl-mock:answer (com.thejach.anansi/example.config:db-name) db-name)
      (com.thejach.anansi/example.db:db-setup)
      (let ((server-thread (bt:make-thread (lambda () (com.thejach.anansi/example:main)))))
        (unwind-protect
          (webdriver:with-session (webdriver:make-capabilities :always-match '((browser-name . "firefox")
                                                                               ; comment out next line if you want non-headless
                                                                               ("moz:firefoxOptions" . (("args" . #("-headless"))))
                                                                               ))
            (&body))

          (com.thejach.anansi/example:stop)
          (bt:destroy-thread server-thread))))))

(test user-registration
  "Happy path test that user registration works and results in auto logging in"
  (with-fixture fresh-session-server-and-db ()
      (navigate-home)
      (let ((registration-page (make-instance 'registration-page)))
        (enter-username registration-page "bobby")
        (enter-password registration-page "secret")
        (enter-password2 registration-page "secret")
        (click-submit-button registration-page))
      (is-true (on-welcome-page?))))

(test user-register-logout-login
  "Happy path test that user can register, log out, then log back in"
  (with-fixture fresh-session-server-and-db ()
      (navigate-home)
      (let ((registration-page (make-instance 'registration-page)))
        (enter-username registration-page "bobby")
        (enter-password registration-page "secret")
        (enter-password2 registration-page "secret")
        (click-submit-button registration-page))
      (is-true (on-welcome-page?))
      (logout)
      (is-true (on-home-page?))
      (let ((login-page (make-instance 'login-page)))
        (enter-username login-page "bobby")
        (enter-password login-page "secret")
        (click-submit-button login-page))
      (is-true (on-welcome-page?))))

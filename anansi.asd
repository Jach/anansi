(asdf:defsystem "anansi"
  :description "Rate-limited function calls with special support for mitigating potential denial of service on web login forms."
  :version "0.1.0"
  :author "Jach <github@thejach.com>"
  :license "Unlicense / Public Domain"
  :depends-on ("bordeaux-threads"
               "luckless"
               "trivial-garbage"
               "log4cl")
  :serial t
  :pathname "src"
  :components ((:file "packages")
               (:file "core")
               (:file "login-rate-limiter"))
  :in-order-to ((asdf:test-op (asdf:test-op "anansi/test"))))


(asdf:defsystem "anansi/example"
  :description "A small-ish example providing a login + registration form and showcasing
                use of bcrypt wrapped in the anansi limiter in the authentication file."
  :depends-on ("anansi"
               "cl-bcrypt"

               "hunchentoot"
               "easy-routes"
               "spinneret"

               "str"
               "cl-dbi"
               "dbd-sqlite3")
  :serial t
  :pathname "example"
  :components ((:file "config")
               (:file "db")
               (:file "authentication")
               (:file "web")
               (:file "main")))


(asdf:defsystem "anansi/test"
  :depends-on ("anansi"
               "anansi/example"
               "fiveam"
               "cl-webdriver-client"
               "cl-mock"

               "ironclad"
               "str")
  :serial t
  :pathname "test"
  :components ((:file "package")
               (:file "limiter-tests")
               (:file "login-tests")
               (:file "page-objects")
               (:file "webdriver-tests"))
  :perform (asdf:test-op (o c) (uiop:symbol-call ':5am '#:run-all-tests ':summary ':suite)))


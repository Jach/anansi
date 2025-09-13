(defpackage #:com.thejach.anansi/example.db
  (:use #:cl)
  (:local-nicknames (#:config #:com.thejach.anansi/example.config))
  (:export #:db-setup

           #:with-connection
           #:insert-user
           #:select-id-password-hash))
(in-package #:com.thejach.anansi/example.db)

(defun execute-script (conn script)
  "Executes each statement in script (statements separated by a semicolon followed by a newline)
   as a series of single statement executions. If any result in an error the error will be raised
   with an option to skip and continue.
   The returned value is a list of results from each statement, which is a count of rows affected by that statement."
  (let ((comments-pattern "--.*?(?=\\r?\\n|$)")
        (statements-sep ";\\s*\\r?\\n"))
    (loop for statement in (remove-if #'str:empty?
                                      (mapcar #'str:trim
                                              (cl-ppcre:split statements-sep
                                                              (cl-ppcre:regex-replace-all comments-pattern script (string #\newline)))))
          collect
          (with-simple-restart (skip-and-continue "Skip statement <~a> and continue executing the rest of the script." statement)
            (cl-dbi:do-sql conn statement)))))

(defmacro with-connection ((conn) &body body)
  "Create and use a DB connection with per-connection pragmas taken care of."
  `(cl-dbi:with-connection (,conn :sqlite3 :database-name (config:config :db-name))
     (execute-script ,conn
                     "PRAGMA foreign_keys=ON;
                      PRAGMA busy_timeout=5000")
     ,@body))

(defun db-setup ()
  "Run once to create a fresh db for the example"
  (with-connection (conn)
    (execute-script conn
                    "PRAGMA journal_mode=WAL;
                     PRAGMA auto_vacuum=INCREMENTAL; --instead of default of NONE
                     PRAGMA synchronous=NORMAL; --slightly less safe than waiting with default of FULL, but faster

                     DROP TABLE IF EXISTS users;
                     CREATE TABLE IF NOT EXISTS users (
                       id INTEGER PRIMARY KEY AUTOINCREMENT,
                       username TEXT NOT NULL UNIQUE,
                       password_hash TEXT NOT NULL
                     );
                     -- No need for following statement, sqlite creates an implicit index for the unique constraint,
                     -- and repurposes the built-in rowid index for the primary id.
                     --CREATE UNIQUE INDEX IF NOT EXISTS idx_users_username ON users(username);
                     ")))

;; verify indexes if you want
#+nil
(progn
  ;; should see sqlite_autoindex_users_1
  (with-connection (conn)
    (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare conn "SELECT name, tbl_name, sql
                                                            FROM sqlite_master
                                                            WHERE type = 'index';"
                                                      ))))
  ;; same result, just a bit more other info
  (with-connection (conn)
    (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare conn "PRAGMA index_list(users)"))))

  ;; should see it's on the username column
  (with-connection (conn)
    (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare conn "PRAGMA index_info('sqlite_autoindex_users_1')"))))

  ;; should see it's using rowid
  (with-connection (conn)
    (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare conn "EXPLAIN QUERY PLAN SELECT * FROM users WHERE id = 2"))))

  ;; should see it's using index sqlite_autoindex_users_1
  (with-connection (conn)
    (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare conn "EXPLAIN QUERY PLAN SELECT * FROM users WHERE username = 'bob'"))))
  )

(defun insert-user (conn username password-hash)
  "Inserts a new user"
  (let ((res (cl-dbi:do-sql conn "INSERT INTO users (username, password_hash) VALUES (?, ?)" (list username password-hash))))
    (assert (eql res 1) () "Error: user insert did not result in a new row")
    res))

(defun select-id-password-hash (conn username)
  "Selects the user with the given username. Returns a hash table with keys 'id' and 'password_hash'.
   If the username cannot be found, an empty hash table is returned."
  (let* ((query (cl-dbi:prepare conn "SELECT id, password_hash FROM users WHERE username = ?" ))
         (query-result (cl-dbi:execute query (list username)))
         (row (cl-dbi:fetch query-result :format :hash-table)))
    row))

(defun %dump-users (conn)
  (cl-dbi:fetch-all (cl-dbi:execute (cl-dbi:prepare conn "SELECT * FROM users")) :format :hash-table))

#+nil
(with-connection (conn) (%dump-users conn))


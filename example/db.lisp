(defpackage #:com.thejach.anansi/example.db
  (:use #:cl)
  (:local-nicknames (#:config #:com.thejach.anansi/example.config))
  (:export #:db-setup

           #:with-connection
           #:insert-user
           #:select-id-password-hash))
(in-package #:com.thejach.anansi/example.db)

;; I've tried to make this file serve as a future example (mainly to myself) of "production ready" sqlite usage.
;; Some remarks about pragmas are in the code and I've left a few additional notes at the bottom of the file.

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

(defmacro with-connection ((conn &key writer?) &body body)
  "Create and use a DB connection with per-connection pragmas taken care of.

   If the connection is going to perform writes, then the keyword :writer? should be specified as T.
   This results in a BEGIN IMMEDIATE; statement being executed immediately after the pragmas, and a COMMIT; statement being executed after the body.
   The reason is to prevent the edge case where a connection issues an explicit BEGIN, then does some reads, then attempts a write,
   and when sqlite attempts to upgrade the transaction, the write can immediately fail with a busy database error if another
   connection already has the write lock. In other words, it ignores the busy_timeout. With BEGIN IMMEDIATE, however, the timeout is honored."
  (let ((body-result (gensym)))
    `(cl-dbi:with-connection (,conn :sqlite3 :database-name (config:db-name))
       (execute-script ,conn
                       "PRAGMA foreign_keys=ON;
                        PRAGMA busy_timeout=5000;")
       ,@(when writer?
           `((cl-dbi:do-sql ,conn "BEGIN IMMEDIATE;")))

       (let ((,body-result
               ,@body))

         ,@(when writer?
             `((cl-dbi:do-sql ,conn "COMMIT;")))
         (cl-dbi:do-sql ,conn "PRAGMA optimize;")

         ,body-result))))

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
                     ) STRICT; --STRICT is not particularly helpful here since the columns are TEXT, but good practice since 2022 (https://www.sqlite.org/stricttables.html)
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

;; Some extra notes on sqlite follow.

;; Some people (https://kerkour.com/sqlite-for-servers) suggest setting up a connection pool with N reader connections and 1 writer connection.
;; However in practice this seems to be much less necessary than for a remote process database. Sqlite's own forums use a classic one-connection-per-request model, even.
;; In this example app, I go even further and use one connection per statement.

;; Distinguishing between reader and writer connections can be valuable, though, due to an edge case where a connection starts out reading and then tries to
;; upgrade to write. This is described well in https://berthub.eu/articles/posts/a-brief-post-on-sqlite3-database-locked-despite-timeout/
;; So I added an optional :writer? keyword to my with-connection macro. In this app's case, it wasn't really necessary, because everything was already a
;; single-statement transaction, but it's good to be aware of the problem for the future.

;; Additionally, with-connection executes some pragmas that must be set per connection.
;; The most important is the busy_timeout setting.
;; The foreign_keys pragma isn't needed in this small application, but I set it anyway because otherwise foreign key constraints won't be honored.
;; sqlite recommends the optimize pragma (https://www.sqlite.org/pragma.html#pragma_optimize) to appear just prior to closing each connection, for short-lived
;; connections, so I set that too.

;; There are other per-connection pragmas that are not set or executed that are still worth considering for other applications.
;; recursive_triggers=ON if using triggers.
;;
;; cache_size. By default it's around 2MB. Increasing it (set it to a negative number for KB, e.g. -64000 would be around 64 MB; set it to a positive number for
;; pages, with each page being the size of the page_size pragma) may offer some slight performance benefits, but keep in mind if the db fits in the filesystem
;; cache already, the benefit is just from not having to go to the kernel.
;;
;; mmap_size. By default it's off, but setting it to a number of bytes can significantly increase performance for reads. Try 256 MB? Or measure the size of a
;; working set, or even the whole DB.
;;
;; temp_store=MEMORY. By default temp tables/indices will be stored with files, using an in-memory store could be faster.

;; Finally, some per-db pragmas are set in the setup.
;; Most important is journal_mode=WAL to have concurrent readers and writer.
;; auto_vacuum must be turned on before tables are created, otherwise a manual VACUUM must be done before changing back and forth from NONE.
;; In INCREMENTAL mode, auto-vacuuming isn't done automatically but can freely be made to be done by switching to FULL or using the incremental_vacuum pragma.
;; A manual VACUUM will still be a good idea to limit fragmentation.
;; synchronous=NORMAL adds a bit of performance by syncing less than the default FULL mode, the tradeoff is that in WAL mode, durability is lost in that a
;; transaction that commits just before power loss or system crash may be rolled back on reboot.

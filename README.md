# Anansi
[![Build Status](https://jenkins.thejach.com/buildStatus/icon?job=anansi&style=plastic)](https://jenkins.thejach.com/job/anansi/)

```
     \    |    /
------\---+---/------
       \  |  /
---=====[ * ]=====---
       /  |  \
------/---+---\------
     /    |    \
```

This library provides a way to rate-limit function calls with a special configurable login verification component for use in a web application. The main purpose
of the login component is to augment the standard verification flow of "Take password -> hash -> compare with stored DB hash" with some protections against
server abuse, specifically denial of service (DoS).

This becomes more important when using a hash function like bcrypt, because the whole point of such hash functions is to be relatively slow to compute. That
way, if a large database table is compromised an attacker can't feasibly try to crack every record's hash even with common passwords, which they could trivially
do if a faster hash like sha256 was used.

Of course, another common solution to this problem is to just run the login/registration/other auth stuff on an entirely separate machine, or at least in a
separate process that can have various high-level Linux tools apply CPU/memory/etc. limits to directly. But I like the idea of everything in one Lisp image when
practical.

# API

The anansi project contains the following systems:

* anansi
* anansi/test
* anansi/example
* anansi/web-test

The `anansi` system creates the following packages and exported symbols:

* `com.thejach.anansi`
    * `limiter` -- the general limiter class
    * `make-limiter` -- an optional wrapper constructor around `make-instance`
        * Allows for 5 keyword options: `:computation` (your zero-argument function), `:constant-runtime` (fixed time of execution), `:jitter` (random max of extra jitter time),
          `:concurrency` (how many computations can run at once), and `:max-wait` (how long a computation can wait before starting to run)
    * `compute` -- the core method to call which may or may not invoke the underlying computation function, but will execute in fixed constant-runtime+jitter regardless
        * Takes two optional parameters: `override-computation` is a function that will be called instead of the underlying computation (if given) stored at
          object creation time; `immediately-wait?` will result in the call sleeping for the expected constant-runtime+jitter duration without doing anything
          else.
        * Besides the `override-computation` argument, you can also customize or change the underlying computation with the `.computation` accessor, or the
          `with-computation` macro.
    * `compute-result` -- the result struct returned by `compute`, with the following 3 accessors:
        * `compute-result-underlying-finished?` -- true only if the underlying function was successfully called
        * `compute-result-underlying-result` -- if the underlying function was called, this is the value that was returned
        * `compute-result-final-status` -- a keyword, will be `:succeeded` if `underlying-finished?` is true, otherwise will be some other keyword indicating a failure reason.
    * `login-rate-limiter` -- a further specialized version of the general limiter class with options for banning IPs or locking out user ids if compute (login)
      attempts are too frequent.
    * `make-login-rate-limiter` -- an optional wrapper constructor around `make-instance`
        * Besides the keys to `make-limiter`, allows for 5 other options: `:max-attempts-per-minute` (rate that if exceeded results in an IP ban),
          `:ban-duration-minutes` (how long an IP ban lasts), `:max-user-failures-per-minute` (rate of attempts by any IP against a particular user id before
          that user id is locked out), `:lock-user-duration-minutes` (how long a user id can be locked out), `:cleanup-interval-minutes` (how often a background
          maintenance thread runs to manage unbanning/unlocking tasks and keep the records from growing without bound)
    * `verify-login` -- the core method to call, takes additional required arguments `user-key` and `ip`. Invokes the underlying `compute` if and only if the given
      arguments aren't for a banned ip or locked user.
        * If the given `user-key` and `ip` are both `nil`, then this behaves the same as `compute`. You can leave just one `nil` to only get the other's
          behavior.
        * Takes two optional parameters: `override-computation` is again a function that will be used instead of any stored underlying function;
          `drop-immediately?` when true will result in the function call returning immediately for a banned user or locked account instead of continuing to
          sleep for the constant-runtime+jitter duration.
    * `.registry` -- a registry of Prometheus metrics (themselves available with `.stored-metrics`) associated with the limiter object.
    * `*logger*` -- a function taking a log level and message string, expected to be `setf`'d by application code to receive various log messages from this
      library into the application's preferred logging framework.

## Example

An example web service showcasing a user registration/login flow is made in the `anansi/example` system. The two most important files to look at are:

* [`example/authentication.lisp`](example/authentication.lisp) -- here a login-rate-limiter is created as a singleton shared by the login and registration flows. It is used by two functions
  for the two flows to wrap the expensive bcrypt computation/check within the limiter, with the registration side only checking rate limits for IPs.
* [`example/web.lisp`](example/web.lisp) -- the two `defroute` forms for `login` and `register` near the bottom of the file handle the POST requests for their respective forms. Various data validation is done
  and ultimately the login flow calls `auth:verify-login` with a given user id, IP address, password, and password hash, and shows how to use the
  returned `compute-result-...` to extract various information beyond a plain pass/fail. Similarly for the registration flow, but it calls `auth:generate-hash`
  passing the password to hash and the IP address.

To run, first load the example system with `(ql:quickload "anansi/example")`.
Next, you should create the default sqlite database by evaluating `(com.thejach.anansi/example.db:db-setup)`. It will create db.sqlite3 in the example folder.
Finally, evaluate `(bt:make-thread #'com.thejach.anansi/example:main)` to start the example webapp on port 56142 within its own thread.

# Design Notes

All calls to `compute` or `verify-login` must finish in a constant runtime / deadline D (default 1s) plus jitter J (default random up to 0.1s), regardless of whether the login check or whatever other computation specified is successful or not. An attacker ideally should not be able to
distinguish between failure because of incorrect password, failure because a user doesn't exist, failure because the computation was skipped due to dropping
their requests for abuse or due to server load, or timing information that might be related to the length of either the password or the hashed value.

A semaphore is used to restrict the number of computations that can happen at once to some fixed limit K (default 4). If this limit is
hit, new requests will wait some amount of time W (max-wait, default 0.6s) for the semaphore. The time to complete a computation B (estimated by measuring the time of the first run) plus the max semaphore wait time W should be less than D. The
thread will sleep at the end to consume any extra time after semaphore waiting/hashing that remains until D total duration + jitter time is hit.

A bounded queue in front of the semaphore provides hard back-pressure to ensure that under extra load, requests will immediately fail to a sleep state for the
deadline duration + jitter instead of fighting each other in sleep-wake-sleep loops for the semaphore. This bounded queue is actually implemented as just another
semaphore but with a timeout of the smallest possible value.

Some numbers for a default configuration:

* Let D = 1000ms be the deadline that all login requests must finish in, whether success or fail
* Let J = 0-100ms be random jitter added to the total request response time
* Let K = 4 be the hash concurrency, or the number of parallel hashes that can happen on the server at once
* Let B = 220ms be the typical hash time of the given hash algorithm like bcrypt. On relatively recent AMD processors, with bcrypt this is achieved around 12 rounds.
* Let W = 600ms be the maximum semaphore wait time that a thread can wait to start performing the hash computation before giving up with a failure

* Peak service rate is μ = K / B = 4 / 0.220 = 18.18 requests/second

To guarantee that any job that gets enqueued will start within W, we set a bound on extra waiting jobs:

* Q\_max = floor(K * (W/B)) - 1 = 9

(The -1 is to ensure that by the time the last slot in the queue is served, it hasn't waited more than W.)

Thus at most 4 are computing bcrypt + 9 are waiting, total in system is thus <= 13. If a 14th request arrives while all these slots are occupied, it immediately enters
a sleep state for the duration ~D+J instead of contending for the semaphore.

## Secondary protections of the login-rate-limiter

Given an IP address, track the request rate from that IP. If it exceeds some amount, then subsequent requests from that IP are temporarily "banned" and
immediately go into the sleep-until-deadline state, there won't be any hashing even if the system is otherwise free. The duration of the ban is fixed with a
default of 30 minutes.

Given a user-key, which may represent a user id or a username or an email address or a UUID, basically some unique identifier for unique users, track the request rate for login attempts on that specific user regardless of IP. Again, if it exceeds
some amount, then subsequent requests on that user by any IP are temporarily locked and go into the sleep-until-deadline state for the duration of the lock. The duration is fixed with a default of 10 minutes.

It may be more desirable to have shorter bans/locks with exponentially increasing times for repeat offenders. However I would suggest instead setting up a
higher level rule with a service like fail2ban which reads logs about bans and can apply its own ban logic to block requests before they even hit the Lisp web
server. As for user locks, you may wish to notify the legitimate user via an email that their account is undergoing an attack and that if they need to login again
they may be unable to while the attack persists.

# Logging

At various moments in `core.lisp` and `login-rate-limiter.lisp` the function `log` is called with various information. For example, logging that a particular IP
address was banned. The `*logger*` special variable can be `setf`'d by application code (see the example's `main.lisp` for doing it with vom). This may be
especially useful to push logs to a log file which can be further processed by system-level tools like fail2ban to drop bad-actor IPs before they even hit the
Lisp application, or apply more complex ban schemes like exponential timeouts, and so on.

# Metrics

[Prometheus.cl](https://github.com/deadtrickster/prometheus.cl) is used to store various metrics. The metrics registry for a limiter object can be accessed with
`.registry`. The metrics themselves are in a `.stored-metrics` hash table. By default, these metrics are defined and tracked for the `limiter` class:

* `limiter_compute_duration_seconds`
* `limiter_compute_successes`
* `limiter_compute_failures` -- note this has a `final_status` label indicating the failure reason

The `login-rate-limiter` has six additional metrics:

* `login_rate_limiter_verify_successes` -- note this isn't the same as the underlying computation successfully running or not, but whether such computation ran
  and returned something true
* `login_rate_limiter_verify_failures`
* `login_rate_limiter_banned_ips_total`
* `login_rate_limiter_unbanned_ips_total`
* `login_rate_limiter_locked_users_total`
* `login_rate_limiter_unlocked_users_total`

The example web application exposes these on the `/metrics` route.

# License

Anansi is released under the Unlicense and thus is in the public domain.

# Name

> Anansi ... is a character in Akan religion and folklore associated with stories, wisdom, knowledge, and trickery, most commonly depicted as a spider.

The graphic at the top is a poor attempt at an ascii spider in a web...

# Anansi

```
     \    |    /
------\---+---/------
       \  |  /
---=====[ * ]=====---
       /  |  \
------/---+---\------
     /    |    \
```

Current status: WIP

This library provides a way to rate-limit function calls with a special configurable login verification component for use in a web application. The main purpose
of the login component is to augment the standard verification flow of "Take password -> hash -> compare with stored DB hash" with some protections against
server abuse, specifically denial of service (DoS).

This becomes more important when using a hash function like bcrypt, because the whole point of such hash functions is to be relatively slow to compute. That
way, if a large database table is compromised an attacker can't feasibly try to crack every record's hash even with common passwords, which they could trivially
do if a faster hash like sha256 was used.

# API

The anansi project creates the following systems:

* anansi
* anansi/test

The `anansi` system creates the following packages and key exported symbols:

* `com.thejach.anansi`
    * `limiter` -- the limiter class
    * `make-limiter` -- wrapper constructor around `make-instance`
        * Expects keys :computation (your zero-argument function), :constant-runtime (fixed time of execution), :jitter (random max of extra jitter time),
          :concurrency (how many computations can run at once), and :max-wait (how long a computation can wait before starting to run)
    * `compute` -- key method to call which may or may not invoke the underlying computation function, but will execute in fixed constant-runtime+jitter regardless
    * `compute-result` -- result struct returned by `compute`, with the following 3 accessors:
        * `compute-result-underlying-finished?`
        * `compute-result-underlying-result`
        * `compute-result-final-status`

# Core Design

All calls to `verify-login` must finish in a constant runtime / deadline D plus jitter J, regardless of whether the login is successful or not. An attacker should not be able to
distinguish between failure because of incorrect password, failure because a user doesn't exist, failure because the computation was skipped due to dropping
their requests for abuse or due to server load, or timing information that might be related to the length of either the password or the hashed value.

A semaphore is used to restrict the number of bcrypt (or other hash) computations that can happen at once to some fixed limit K, default of 4. If this limit is
hit, new requests will wait some amount of time W for the semaphore. The time to complete a hash B plus the max semaphore wait time W should be less than D. The
thread will sleep at the end to consume any extra time after semaphore waiting/hashing that remains until D total duration is hit.

A bounded queue in front of the semaphore provides hard back-pressure to ensure that under extra load, requests will immediately fail to a sleep state for the
deadline duration instead of fighting each other in sleep-wake-sleep loops for the semaphore. This bounded queue is actually implemented as just another
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

Thus at most 4 are in bcrypt + 9 are waiting, total in system is thus <= 13. If a 14th request arrives while all these slots are occupied, it immediately enters
a sleep state for the duration ~D instead of contending for the semaphore.

# Secondary protections of the login component

Given an IP address, track the request rate from that IP. If it exceeds some amount, then subsequent requests from that IP are temporarily "banned" and
immediately go into the sleep-until-deadline state, there won't be any hashing even if the system is otherwise free. The duration of the ban starts at 30
seconds but increases exponentially.

Given a user-key, which may represent a username or email address or UUID, track the request rate for that specific user regardless of IP. Again if it exceeds
some amount, then subsequent requests on that user are temporarily banned and go into the sleep-until-deadline state for a ban duration. The duration of this
ban also starts at 30 seconds but doubles until it hits a maximum of 24 hours.

Both ban events are logged at the INFO level with log4cl, which may be of value for server-level protections like fail2ban or resolving issues where someone's
account is being targeted and the abusive cases are preventing their legitimate login.

# Metrics

[Prometheus.cl](https://github.com/deadtrickster/prometheus.cl) is used to store various metrics. They can be found and exposed via `*anansi-registry*`.

# Example

A small example site that's just a login form is provided by the `anansi-web-login/example` system. There are also some selenium webdriver tests for this system
in `anansi-web-login/test`.

# License

Anansi is released under the Unlicense and thus is in the public domain.

# Name

> Anansi ... is a character in Akan religion and folklore associated with stories, wisdom, knowledge, and trickery, most commonly depicted as a spider.

The graphic at the top is a poor attempt at an ascii spider in a web...

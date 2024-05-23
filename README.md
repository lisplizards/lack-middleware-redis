# lack-middleware-redis

## Usage

Wrap app:

```lisp
(funcall lack/middleware/redis:*lack-middleware-redis*
         *app*
         :pools '((:pool-id :cache
                   :host "redis01.example.com"
                   :max-open-count 10
                   :max-idle-count 4
                   :timeout 2000
                   :idle-timeout 40000)))
```

Lack Builder:

```lisp
(lack:builder
 (:redis :pools '((:pool-id :cache
                   :host "redis01.example.com"
                   :port 6379
                   :max-open-count 10
                   :max-idle-count 4)
                  (:pool-id :jobs
                   :host "redis02.example.com"
                   :max-open-count 12)))
 *app*)
```

Once you have a pool configured, you can call macro `WITH-REDIS` from your application to checkout a connection from the specified pool:

```common-lisp
(with-redis (:cache)
  (red:set "foo" "bar"))
```

### Pool options

* `POOL-ID`: required; unique name of the pool, a keyword
* `POOL-NAME`: optional; passed through to the `ANYPOOL:MAKE-POOL` as the `NAME` parameter, defaulting to "redis-[downcased-pool-id]" if not provided
* `HOST`: optional; the hostname of the Redis server (default: 127.0.0.1)
* `PORT`: optional the port number of the Redis server; (default: 6379)
* `AUTH`: optional; password to authenticate to the Redis server
* `MAX-OPEN-COUNT`: optional; passed through to `ANYPOOL:MAKE-POOL` (default: 10)
* `MAX-IDLE-COUNT`: optional; passed through to `ANYPOOL:MAKE-POOL` (default 4)
* `TIMEOUT`:  optional; passed through to `ANYPOOL:MAKE-POOL`; milliseconds to wait checking out a connection once MAX-OPEN-COUNT is reached (default: 2000)
* `IDLE-TIMEOUT`: optional; passed through to `ANYPOOL:MAKE-POOL` (default: 30000)
* `ON-TOO-MANY-OPEN-CONNECTIONS`: optional; function called from `HANDLER-BIND` handler for `ANYPOOL:TOO-MANY-OPEN-CONNECTION` error before returning a 503 response; may be useful for publishing a metric or some other purpose

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-middleware-redis)
```

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Dependencies

### Middleware

* [anypool](https://github.com/fukamachi/anypool)
* [cl-redis](https://github.com/vseloved/cl-redis)

### Tests

* [rove](https://github.com/fukamachi/rove)
* [bordeaux-threads](https://github.com/sionescu/bordeaux-threads)

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0


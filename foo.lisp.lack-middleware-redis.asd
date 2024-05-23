;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-redis"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("anypool"
               "cl-redis")
  :components ((:module "src"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Lack middleware for Redis connection pooling"
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-redis/tests"))))

(defsystem "foo.lisp.lack-middleware-redis/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("bordeaux-threads"
               "foo.lisp.lack-middleware-redis"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-middleware-redis"
  :perform (test-op (op c) (symbol-call :rove :run c)))

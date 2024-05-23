;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:lack/middleware/redis
  (:use #:cl)
  (:export #:*lack-middleware-redis*
           #:*redis-pools*
           #:pool-not-defined-error
           #:with-redis))

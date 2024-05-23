;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/redis/tests)

(setup
 (redis:with-connection (:host "localhost" :port 6379)
   (red:del "lack-middleware-redis-test:*")))

(deftest middleware
    (testing "checks out a pooled Redis connection and can send commands"
             (flet ((app (env)
                      (declare (ignore env))
                      (lack/middleware/redis:with-redis (:middleware-test)
                        (ok (boundp 'redis:*connection*))
                        (ok (not (null redis:*connection*)))
                        (ok (equal "OK" (red:set "lack-middleware-redis-test:foo" "bar")))
                        (ok (equal "OK" (red:set "lack-middleware-redis-test:baaz" "quux")))
                        (ok (equal "OK" (red:set "lack-middleware-redis-test:bar" "foo")))
                        (ok (equal "bar" (red:get "lack-middleware-redis-test:foo")))
                        (ok (equal "quux" (red:get "lack-middleware-redis-test:baaz")))
                        (ok (equal "foo" (red:get "lack-middleware-redis-test:bar")))
                        (ok (equal "quux" (red:echo "quux")))
                        `(200
                          (:content-type "text/plain"
                           :content-length 13)
                          ("Hello, World.")))))
               (let ((app (funcall lack/middleware/redis:*lack-middleware-redis*
                                   #'app
                                   :pools '((:pool-id :middleware-test
                                             :pool-name "redis-pool-middleware-test"
                                             :max-open-count 3)))))
                 (funcall app ()))))

  (testing "signals POOL-NOT-DEFINED-ERROR when an unknown pool is given to WITH-REDIS"
           (flet ((app (env)
                    (declare (ignore env))
                    (lack/middleware/redis:with-redis (:mypool)
                      `(200
                        (:content-type "text/plain"
                         :content-length 13)
                        ("Hello, World.")))))
             (let ((app (funcall lack/middleware/redis:*lack-middleware-redis*
                                 #'app
                                 :pools '((:pool-id :middleware-test
                                           :pool-name "redis-pool-middleware-test"
                                           :host "localhost"
                                           :max-open-count 3)))))
               (ok (signals (funcall app ())
                            'lack/middleware/redis:pool-not-defined-error)))))

  (testing "returns a 503 response when the maximum open connection count has been reached at conclusion of the timeout limit (timeout: 0)"
           (flet ((app (env)
                    (declare (ignore env))
                    (lack/middleware/redis:with-redis (:mypool)
                      (sleep 3)
                      (red:set "lack-middleware-redis-test:hello" "world")
                      `(200
                        (:content-type "text/plain"
                         :content-length 13)
                        ("Hello, World.")))))
             (let ((app (funcall lack/middleware/redis:*lack-middleware-redis*
                                 #'app
                                 :pools '((:pool-id :mypool
                                           :host "localhost"
                                           :port 6379
                                           :max-open-count 2
                                           :max-idle-count 4
                                           :timeout 0))))
                   (failure-count 0))
               (flet ((make-thread ()
                        (bt2:make-thread (lambda ()
                                           (let ((response (funcall app ())))
                                             (when (= 503 (first response))
                                               (incf failure-count))
                                             response)))))
                 (let ((threads (list (make-thread)
                                      (make-thread)
                                      (make-thread)
                                      (make-thread)
                                      (make-thread))))
                   (dolist (thread threads)
                     (bt2:join-thread thread))
                   (ok (= 3 failure-count)))))))

  (testing
   "does NOT return 503 response when the maximum open connection count has been reached but pool has availability within the timeout limit"
   (flet ((app (env)
            (declare (ignore env))
            (lack/middleware/redis:with-redis (:mypool)
              (sleep 3)
              (red:set "lack-middleware-redis-test:foo" "foofoo")
              `(200
                (:content-type "text/plain"
                 :content-length 13)
                ("Hello, World.")))))
     (let ((app (funcall lack/middleware/redis:*lack-middleware-redis*
                         #'app
                         :pools '((:pool-id :mypool
                                   :host "localhost"
                                   :port 6379
                                   :max-open-count 2
                                   :max-idle-count 4
                                   :timeout 10000))))
           (failure-count 0))
       (flet ((make-thread ()
                (bt2:make-thread (lambda ()
                                   (let ((response (funcall app ())))
                                     (when (= 503 (first response))
                                       (incf failure-count))
                                     response)))))
         (let ((threads (list (make-thread)
                              (make-thread)
                              (make-thread)
                              (make-thread)
                              (make-thread))))
           (dolist (thread threads)
             (bt2:join-thread thread))
           (ok (= 0 failure-count)))))))

  (testing
   "checks out a pooled connection to the redis server and sends commands, from two separate pools, with nesting"
   (flet ((app (env)
            (declare (ignore env))
            (lack/middleware/redis:with-redis (:mypool)
              (ok (boundp 'redis:*connection*))
              (ok (not (null redis:*connection*)))
              (ok (equal "OK" (red:set "lack-middleware-redis-test:foobar" "baaz")))
              (ok (equal "baaz" (red:get "lack-middleware-redis-test:foobar")))
              (ok (equal "quux" (red:echo "quux")))
              (let ((original-connection redis:*connection*))
                (lack/middleware/redis:with-redis (:another-pool)
                  (ok (boundp 'redis:*connection*))
                  (ok (not (null redis:*connection*)))
                  (ok (not (eq original-connection redis:*connection*)))
                  (ok (equal "OK" (red:set "lack-middleware-redis-test:baar" "foo")))
                  (ok (equal "foo" (red:get "lack-middleware-redis-test:baar")))
                  (ok (equal "quux" (red:echo "quux")))
                  `(200
                    (:content-type "text/plain"
                     :content-length 13)
                    ("Hello, World.")))))))
     (let ((app (funcall lack/middleware/redis:*lack-middleware-redis*
                         #'app
                         :pools '((:pool-id :mypool
                                   :host "localhost")
                                  (:pool-id :another-pool
                                   :host "localhost")))))
       (funcall app ())))))

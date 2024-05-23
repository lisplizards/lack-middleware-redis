;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/redis)

(defvar *redis-pools* ())

(defparameter *lack-middleware-redis*
  (lambda (app &key pools)
    (declare (type function app)
             (type list pools))
    (check-type pools list)
    (assert (not (null pools))
            nil
            "POOLS cannot be empty - define at least one pool.")
    (let ((pool-infos ()))
      (declare (type list pool-infos))
      (dolist (pool-spec pools)
        (let ((pool-id (getf pool-spec :pool-id)))
          (check-type pool-id keyword)
          (when (getf *redis-pools* pool-id)
            (error "Non-unique POOL-ID: ~A" pool-id))
          (let ((pool-info
                  (destructuring-bind (&key pool-id
                                         pool-name
                                         host
                                         port
                                         auth
                                         (max-open-count 10)
                                         (max-idle-count 4)
                                         (timeout 2000)
                                         (idle-timeout 30000)
                                         on-too-many-open-connections)
                      pool-spec
                    (declare (ignore host port auth)
                             (ignore max-open-count max-idle-count timeout idle-timeout))
                    (check-type pool-name (or null string))
                    (check-type on-too-many-open-connections (or null function))
                    (let* ((connect-options
                             (loop for (key value) on pool-spec by #'cddr
                                   when (and (member key '(:host :port :auth))
                                             value)
                                     append (list key value)))
                           (pool-options
                             (loop for (key value) on pool-spec by #'cddr
                                   when (and (member key '(:max-open-count
                                                           :max-idle-count
                                                           :timeout
                                                           :idle-timeout))
                                             value)
                                     append (list key value)))
                           (pool (apply #'anypool:make-pool
                                        (append
                                         (list
                                          :name (or pool-name (format nil "redis-~A"
                                                                      (string-downcase pool-id)))
                                          :connector (lambda ()
                                                       (apply #'make-instance
                                                              'redis:redis-connection
                                                              connect-options))
                                          :disconnector (lambda (connection)
                                                          (declare (type redis:redis-connection connection))
                                                          (let ((redis:*connection* connection))
                                                            (declare (type redis:redis-connection
                                                                           redis:*connection*))
                                                            (redis:disconnect)))
                                          :ping (lambda (connection)
                                                  (declare (type redis:redis-connection connection))
                                                  (let ((redis:*connection* connection))
                                                    (declare (type redis:redis-connection redis:*connection*))
                                                    (red:ping))))
                                         pool-options))))
                      (make-instance 'pool-info
                                     :pool pool
                                     :on-too-many-open-connections on-too-many-open-connections)))))
            (setf (getf pool-infos pool-id)
                  pool-info))))
      (lambda (env)
        (declare (optimize (speed 3) (safety 0) (debug 0))
                 (type list env))
        (let ((*redis-pools* pool-infos))
          (declare (type list *redis-pools*))
          (funcall app env))))))

(defmacro with-redis ((pool-id) &body body)
  (let ((gensym-pool-id (gensym "pool-id")))
    `(let* ((,gensym-pool-id ,pool-id)
            (pool-info (getf lack/middleware/redis:*redis-pools* ,gensym-pool-id)))
       (declare (type keyword ,gensym-pool-id)
                (type (or null lack/middleware/redis::pool-info) pool-info))
       (or pool-info (error 'pool-not-defined-error :pool-id ,gensym-pool-id))
       (with-slots (pool on-too-many-open-connections)
           pool-info
         (declare (type lack/middleware/redis::pool-info pool-info)
                  (type anypool:pool pool)
                  (type (or null function) on-too-many-open-connections))
         (block nil
           (handler-bind ((anypool:too-many-open-connection
                            (lambda (condition)
                              (declare (type anypool:too-many-open-connection condition))
                              (and on-too-many-open-connections
                                   (funcall on-too-many-open-connections condition))
                              (return
                                `(503
                                  (:content-type "text/plain"
                                   :content-length 19)
                                  ("Service Unavailable"))))))
             (anypool:with-connection (redis:*connection* pool)
               ,@body)))))))

(define-condition pool-not-defined-error (error)
  ((pool-id :initarg :pool-id))
  (:report (lambda (condition stream)
             (with-slots (pool-id)
                 condition
               (format stream "No Redis pool defined for pool-id: ~A" pool-id))))
  (:documentation "Error signalled when attempting to connect to an unknown pool from WITH-REDIS."))

(defclass pool-info ()
  ((pool :reader pool
         :initarg :pool
         :type anypool:pool)
   (on-too-many-open-connections :reader on-too-many-open-connections
                                 :initarg :on-too-many-open-connections
                                 :type (or null function))))

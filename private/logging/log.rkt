#lang racket/base

(provide log?
         log!
         topic
         level)

(define level 'debug)
(define topic 'vestige)

(define (log?)
  (log-level? (current-logger) level topic))

(define (log! message)
  ;; log? already tested
  (log-message (current-logger)
               level
               topic
               message
               (current-continuation-marks)
               #t))

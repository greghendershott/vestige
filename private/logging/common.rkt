#lang racket/base

(require syntax/parse/define)

(provide cms->common-data
         with-more-logging-info)

;; Information like timing and thread that is relevant for simple
;; message logging as well as tracing.

(define key (make-continuation-mark-key 'common))

(define (cms->common-data cms)
  (continuation-mark-set-first cms key))

(define-simple-macro (with-more-logging-info e:expr)
  (let ([data (hasheq 'msec   (current-inexact-milliseconds)
                      'thread (current-thread))])
    (with-continuation-mark key data e)))

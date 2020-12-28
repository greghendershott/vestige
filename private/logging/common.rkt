#lang racket/base

(require syntax/parse/define)

(provide common-key
         logging-common-data
         call-with-more-logging-info
         with-more-logging-info)

;; Information like timing and thread that is relevant for simple
;; message logging as well as tracing.

(define common-key (make-continuation-mark-key 'common))

(define (logging-common-data cms)
  (continuation-mark-set-first cms common-key))

(define-simple-macro (with-more-logging-info body:expr ...+)
  (call-with-more-logging-info (λ () body ...)))

(define (call-with-more-logging-info thk)
  (define data (hasheq 'msec   (current-inexact-milliseconds)
                       'thread (current-thread)))
  (with-continuation-mark common-key data (thk)))

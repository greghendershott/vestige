#lang racket/base

(require racket/contract/base
         racket/logging
         racket/match
         racket/pretty
         syntax/parse/define
         (only-in "private/logger.rkt" logger topic level))

(provide logger topic level
         (contract-out
          [call-with-vestiges (-> (-> (vector/c
                                       log-level/c
                                       string?
                                       any/c
                                       (or/c symbol? #f))
                                      any)
                                  (-> any)
                                  any)])
         with-vestiges)

(define (call-with-vestiges interceptor thunk)
  (with-intercepted-logging interceptor thunk #:logger logger level topic))

(define-simple-macro (with-vestiges e:expr ...+)
  (call-with-vestiges pretty-print-message+value
                      (λ () e ...)))

(define pretty-print-message+value
  (match-lambda [(vector _level str val _topic)
                 (pretty-print (list str val))]))

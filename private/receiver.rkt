#lang racket/base

(require racket/pretty
         "logger.rkt")

(define receiver (make-log-receiver logger 'debug))
(define (receive)
  (pretty-print (sync receiver))
  (receive))
(void (thread receive))

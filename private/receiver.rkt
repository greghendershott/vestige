#lang racket/base

(require racket/match
         racket/pretty
         "logger.rkt")

(define receiver (make-log-receiver logger level))
(define (receive)
  (pretty-print (match (sync receiver)
                  [(vector _level message data _topic)
                   (list message data)]))
  (receive))
(void (thread receive))

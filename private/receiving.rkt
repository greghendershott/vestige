#lang racket/base

(require racket/match
         racket/pretty
         "../logging/private/app.rkt"
         "../logging/private/context.rkt"
         "../logging/private/depth.rkt"
         "../logging/private/common.rkt"
         "../tracing/private/logger.rkt")

(define receiver (make-log-receiver (current-logger) level topic 'fatal #f))
(define (receive)
  (pretty-print (match (sync receiver)
                  [(vector level message (? continuation-mark-set? cms) topic)
                   `([message ,message]
                     [topic   ,topic]
                     [level   ,level]
                     [depth   ,(logging-depth cms)]
                     [caller  ,(caller-srcloc cms)]
                     [context ,(context-srcloc cms)]
                     [common  ,(logging-common-data cms)]
                     [tracing ,(tracing-data cms)])]
                  [(vector level message _data topic)
                   `([message ,message]
                     [topic   ,topic]
                     [level   ,level])]))
  (receive))
(void (thread receive))

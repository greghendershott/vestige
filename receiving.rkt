#lang racket/base

(require racket/match
         racket/pretty
         "private/logging/app.rkt"
         "private/logging/context.rkt"
         "private/logging/depth.rkt"
         "private/logging/common.rkt"
         "private/logging/performance.rkt"
         (rename-in "private/tracing/logger.rkt"
                    [logger tracing-logger]
                    [topic tracing-topic]
                    [level tracing-level]))

(provide log-receiver-vector->hasheq
         serializable-hasheq
         tracing-logger
         tracing-topic
         tracing-level
         ;; Low level instead of using vector->hasheq
         cms->logging-depth
         cms->caller-srcloc
         cms->context-srcloc
         cms->common-data
         cms->tracing-data
         cms->performance-stats
         performance-vectors->hasheq)

(define (log-receiver-vector->hasheq v)
  (match v
    [(vector level message (? continuation-mark-set? cms) topic)
     (hasheq 'message message
             'topic   topic
             'level   level
             'depth   (cms->logging-depth cms)
             'caller  (cms->caller-srcloc cms)
             'context (cms->context-srcloc cms)
             'common  (cms->common-data cms)
             'tracing (cms->tracing-data cms))]
    [(vector level message _unknown-data topic)
     (hasheq 'message message
             'topic   topic
             'level   level
             'depth   0)]))

;; Change as necessary to satisfy jsexpr?
(define (serializable-hasheq h)
  (define (serialize-key k)
    (match k
      [(? symbol?) k]
      [_ (string->symbol (format "~a" k))]))
  (define (serialize-value v)
    (match v
      [(? hash?)   (serializable-hasheq v)]
      [(? list?)   (map serialize-value v)]
      [(? vector?) (map serialize-value (vector->list v))]
      [(? thread?) (format "~a" (object-name v))]
      [(or (? boolean?)
           (? string?)
           (? exact-integer?)
           (and (? inexact-real?)
                (? rational?)))
       v]
      [_ (format "~a" v)]))
  (for/hasheq ([(k v) (in-hash h)])
    (values (serialize-key k)
            (serialize-value v))))

(define (start-log-receiver-thread proc . args)
  (define receiver (apply make-log-receiver args))
  (define (get-event)
    (proc (sync receiver))
    (get-event))
  (thread get-event))

;; For use by things like example.rkt and Scribble documentation.
;; Starts a log receiver thread that pretty-prints jsxpr? hasheqs.
(module+ private
  (module+ start
    (void
     (start-log-receiver-thread (compose pretty-print
                                         log-receiver-vector->hasheq)
                                (current-logger)
                                tracing-level tracing-topic
                                'info 'example
                                'fatal #f))))

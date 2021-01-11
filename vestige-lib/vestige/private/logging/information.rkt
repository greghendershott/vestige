#lang racket/base

(require (for-syntax racket/base
                     "srcloc.rkt")
         syntax/parse/define)

(provide cms->logging-info
         with-more-logging-info
         performance-vectors->hasheq)

;; Information like timing and thread that is relevant for simple
;; message logging as well as tracing.

;; Intentionally not using make-continuation-mark-key.
(define key 'vestige-logging-info-continuation-mark-key)

(define (cms->logging-info cms)
  (continuation-mark-set-first cms key))

(define-syntax-parser with-more-logging-info
  [(_
    (~optional (~seq #:srcloc? srcloc?)
               #:defaults ([srcloc? #'#t]))
    e:expr)
   (quasisyntax/loc this-syntax
     (let ([data
            (hasheq 'srcloc            (and srcloc?
                                            '(#,@(->srcloc-as-list #'e)))
                    'msec              (current-inexact-milliseconds)
                    'thread            (current-thread)
                    'performance-stats (vectors))])
       (with-continuation-mark key data
         e)))])

(define (vectors)
  (define global (make-vector 12))
  (vector-set-performance-stats! global #f)
  (define thread (make-vector 4))
  (vector-set-performance-stats! thread (current-thread))
  (vector global thread))

(define (performance-vectors->hasheq global thread)
  (hasheq 'global
          (for/hasheq ([k (in-list '(current-process-milliseconds
                                     current-milliseconds
                                     current-gc-milliseconds
                                     place-garbage-collections
                                     thread-context-switches
                                     internal-stack-overflows
                                     threads-scheduled-for-execution
                                     syntax-objects
                                     hash-table-searches
                                     additional-hash-slots
                                     machine-code-bytes-allocated
                                     peak-bytes-allocated-before-gc))]
                       [v (in-vector global)])
            (values k v))
          'thread
          (for/hasheq ([k (in-list '(thread-running?
                                     thread-dead?
                                     thread-blocked?
                                     thread-bytes-in-use-for-continuation))]
                       [v (in-vector thread)])
            (values k v))))
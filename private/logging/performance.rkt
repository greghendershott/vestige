#lang racket/base

(require racket/match
         syntax/parse/define)

(provide cms->performance-stats
         performance-vectors->hasheq
         with-performance-stats)

;; Information from vector-set-performance-stats!
;;
;; Like common.rkt but much more detailed.

(define key (make-continuation-mark-key 'performance))

(define (cms->performance-stats cms [proc values])
  (match (continuation-mark-set-first cms key)
    [(vector global thread) (proc global thread)]
    [_ #f]))

(define-simple-macro (with-performance-stats e:expr)
  (with-continuation-mark key (vectors) e))

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

(module+ example
  (define ht (hasheq 'a 0 'b 1))
  (hash-ref ht 'a)
  (hash-ref ht 'b)
  (collect-garbage)
  (with-performance-stats
    (cms->performance-stats (current-continuation-marks)))
  (with-performance-stats
    (cms->performance-stats (current-continuation-marks)
                            performance-vectors->hasheq)))

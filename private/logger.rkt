#lang racket/base

(require racket/format
         racket/list
         racket/match
         "srcloc.rkt"
         "expression-id.rkt")

(provide log-args
         log-results
         logger
         topic
         level)

(define level 'debug)
(define topic 'vestige-trace)
(define logger (make-logger topic (current-logger)))

(define (log! str data)
  (when (log-level? logger level topic)
    (log-message logger
                 level
                 str
                 data
                 #f)))

(define (log-args id args kws kw-vals level)
  (define str
    (match (expression-identifier->string id)
      [(? string? v)  v]
      [_ (~a `(,(syntax-e id) ,@args ,@(append-map list kws kw-vals)))]))
  (log! (~a (make-string (add1 level) #\>) " " str)
        (make-logger-event-value 'call id str level)))

(define (log-results id results level)
  (define str
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  (log! (~a (make-string (add1 level) #\<) " " str)
        (make-logger-event-value 'results id str level)))

(define (make-logger-event-value kind id str level)
  ;; We use hasheq because it is trivial to transform to json via
  ;; jsexpr->string, or to an association list, or whatever.
  ;;
  ;; WARNING: These fields constitute a documented protocol that other
  ;; things use, for example Racket Mode. carefully. Although it is
  ;; fine to add brand new mappings, definitely don't delete any, and
  ;; be attentive to changing the meaning of an existing mapping.
  (hasheq 'kind   kind
          'name   (~a (syntax-e id))
          'level  level
          'show   str
          'srcloc (stx->srcloc id)
          'thread (current-thread)
          'msec   (current-inexact-milliseconds)))

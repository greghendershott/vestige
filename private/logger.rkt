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

;; TODO: For trace-expression the called loc is trivially the same as
;; the defined loc, so just use the latter for both (esp b/c the
;; former can be "vague" when it was obtained from cms->context).

(define (log-args id args kws kw-vals level caller)
  (define str
    (match (expression-identifier->string id)
      [(? string? v)  v]
      [_ (~a `(,(syntax-e id) ,@args ,@(append-map list kws kw-vals)))]))
  (log! (~a (make-string (add1 level) #\>) " " str)
        (make-logger-event-value 'call id str level caller)))

(define (log-results id results level caller)
  (define str
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  (log! (~a (make-string (add1 level) #\<) " " str)
        (make-logger-event-value 'results id str level caller)))

(define (make-logger-event-value kind id str level caller)
  ;; We use hasheq because it is trivial to transform to json via
  ;; jsexpr->string, or to an association list, or whatever.
  ;;
  ;; WARNING: These fields constitute a documented protocol that other
  ;; things use, for example Racket Mode. carefully. Although it is
  ;; fine to add brand new mappings, definitely don't delete any. Be
  ;; be very careful about changing the meaning of an existing mapping
  ;; in a way that seems harmless.
  (hasheq 'kind    kind
          'name    (~a (syntax-e id))
          'level   level
          'show    str
          'defined (stx->srcloc-list id)
          'called  (srcloc->list caller)
          'thread  (current-thread)
          'msec    (current-inexact-milliseconds)))

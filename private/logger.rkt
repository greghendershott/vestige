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


(define (log-args id args kws kw-vals level caller context)
  (define-values (str caller*)
    (match (expression-identifier->string id)
      ;; Traced expressions: 1. Use the expression string. 2. Ignore
      ;; caller loc, using defined loc for that (it will be more
      ;; precise when caller loc was obtained from cms->context).
      [(? string? v)
       (values v id)]
      ;; Traced function calls:
      [_
       (values (~a `(,(syntax-e id) ,@args ,@(append-map list kws kw-vals)))
               caller)]))
  (log! (~a (make-string (add1 level) #\>) " " str)
        (make-logger-event-value 'call id str level caller* context)))

(define (log-results id results level caller context)
  (define str
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  ;; Traced expressions: Always use def loc for caller loc.
  (define caller* (if (expression-identifier->string id) id caller))
  (log! (~a (make-string (add1 level) #\<) " " str)
        (make-logger-event-value 'results id str level caller* context)))

;; -> jsexpr?
(define (make-logger-event-value kind id str level caller context)
  ;; We use hasheq because it is trivial to transform to json via
  ;; jsexpr->string, or to an association list, or whatever.
  ;;
  ;; WARNING: These fields constitute a documented protocol that other
  ;; things use, for example Racket Mode. carefully. Although it is
  ;; fine to add brand new mappings, definitely don't delete any. Be
  ;; be very careful about changing the meaning of an existing mapping
  ;; in a way that seems harmless.
  ;;
  ;; Note conversion of all mapping values to satisfy jsexpr?.
  (hasheq 'kind      (~a kind)
          'name      (~a (syntax-e id))
          'level     level
          'show      str
          'def-site  (->srcloc-as-list id)
          'call-site (and caller (->srcloc-as-list caller))
          'context   (and context (->srcloc-as-list context))
          'thread    (~a (object-name (current-thread)))
          'msec      (current-inexact-milliseconds)))

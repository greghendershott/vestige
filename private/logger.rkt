#lang racket/base

(require racket/format
         racket/list
         racket/match
         syntax/parse/define
         "srcloc.rkt"
         "expression-id.rkt"
         "formals.rkt")

(provide log-args
         log-results
         logger
         topic
         level)

(define level 'debug)
(define topic 'vestige-trace)
(define logger (make-logger topic (current-logger)))

(define (log! message data)
  (log-message logger level topic message data #f))

(define-simple-macro (log-args e:expr ...)
  (when (log-level? logger level topic)
    (do-log-args e ...)))

(define (do-log-args id -tail? args kws kw-vals level -caller context)
  (define-values (show vals caller tail?)
    (match (expression-identifier->string id)
      ;; Traced expressions: 1. Show the expression string. 2. Use
      ;; definition loc as "caller" loc. 3. Disregard any tail-call
      ;; flag.
      [(? string? v)
       (values v v id #f)]
      ;; Traced function calls: 1. Show calling the function with
      ;; plain and keyword args. Use supplied caller loc as-is.
      [_
       (values (~a `(,(syntax-e id) ,@args ,@(append-map list kws kw-vals)))
               (apply ~a #:separator " "
                      (append args (append-map list kws kw-vals)))
               -caller
               -tail?)]))
  (log! (~a (make-string (add1 level) #\>) " " show)
        (make-logger-event-value #t tail? id show vals level caller context)))

(define-simple-macro (log-results e:expr ...)
  (when (log-level? logger level topic)
    (do-log-results e ...)))

(define (do-log-results id results level -caller context)
  (define vals
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  ;; Traced expressions: Use definition loc as "caller" loc.
  (define caller (if (expression-identifier->string id) id -caller))
  (log! (~a (make-string (add1 level) #\<) " " vals)
        (make-logger-event-value #f #f id vals vals level caller context)))

;; Return a hasheq that satisfies jsexpr?
(define (make-logger-event-value call? tail? id show vals level caller context)
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
  (hasheq 'call       call?
          'tail       tail?
          'name       (~a (syntax-e id))
          'level      level
          'show       show
          'values     vals
          'definition (->srcloc-as-list id)
          'formals    (get-formals-stx-prop id)
          'caller     (and caller (->srcloc-as-list caller))
          'context    (and context (->srcloc-as-list context))
          'thread     (~a (object-name (current-thread)))
          'msec       (current-inexact-milliseconds)))

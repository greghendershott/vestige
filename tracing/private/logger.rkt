#lang racket/base

(require racket/format
         racket/list
         racket/match
         racket/string
         syntax/parse/define
         "expression-id.rkt"
         "loc-stx-props.rkt"
         "../../logging/private/srcloc.rkt"
         "../../logging/private/common.rkt")

(provide log-args
         log-results
         logger
         topic
         level
         tracing-key
         tracing-data
         make-tracing-data)

(define tracing-key (make-continuation-mark-key 'vestige/tracing))

(define (tracing-data cms)
   (continuation-mark-set-first cms tracing-key))

(define logger (current-logger))
(define level 'debug)
(define topic 'vestige-tracing)

(define (log! message)
  ;; assumes we already did log-level? test
  (log-message logger
               level
               topic
               message
               (current-continuation-marks)
               #t))

(define-simple-macro (log-args e:expr ...)
  (when (log-level? logger level topic)
    (do-log-args e ...)))

(define (do-log-args id -tail? args kws kw-vals depth)
  (define-values (show vals tail?)
    (match (expression-identifier->string id)
      ;; Traced expressions: Show the expression string. Disregard
      ;; caller loc and tail call flag.
      [(? string? v)
       (values v v #f)]
      ;; Traced function calls:
      [_
       (define vals (string-join (append (map ~v args)
                                         (append-map list
                                                     (map ~a kws)
                                                     (map ~v kw-vals)))))
       (define show (~a "("
                        (syntax-e id)
                        (if (equal? vals "") "" " ")
                        vals
                        ")"))
       (values show vals -tail?)]))
  (with-continuation-mark
    tracing-key
    (make-tracing-data #t tail? id show vals)
    (with-more-logging-info
      (log! (~a (make-string depth #\>) " " show)))))

(define-simple-macro (log-results e:expr ...)
  (when (log-level? logger level topic)
    (do-log-results e ...)))

(define (do-log-results id results depth)
  (define vals
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  (with-continuation-mark
    tracing-key
    (make-tracing-data #f #f id vals vals)
    (with-more-logging-info
     (log! (~a (make-string depth #\<) " " vals)))))

(define (make-tracing-data call? tail? id show vals)
  `([call       ,call?]
    [tail       ,tail?]
    [name       ,(~a (syntax-e id))]
    [show       ,show]
    [values     ,vals]
    [identifier ,(->srcloc-as-list id)]
    [formals    ,(get-formals-stx-prop id)]
    [header     ,(get-header-stx-prop id)]))

#lang racket/base

(require racket/format
         racket/list
         racket/match
         racket/string
         syntax/parse/define
         "expression-id.rkt"
         "loc-stx-props.rkt"
         "../logging/srcloc.rkt"
         "../logging/common.rkt")

(provide log-args
         log-results
         logger
         topic
         level
         tracing-key
         cms->tracing-data
         make-tracing-data)

;;; continuation mark

;; Intentionally not using make-continuation-mark-key because
;; vestige/reciving could be dynamic-required.
(define tracing-key 'vestige-tracing-continuation-mark-key)

(define-simple-macro (with-tracing-mark data e:expr)
  (with-continuation-mark tracing-key data e))

(define (cms->tracing-data cms)
   (continuation-mark-set-first cms tracing-key))

;;; logging

(define logger (current-logger))
(define level 'debug)
(define topic 'vestige/tracing)

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
  (define-values (show in-situ tail?)
    (match (expression-identifier->string id)
      ;; Traced expressions: Show the expression string. Disregard
      ;; caller loc and tail call flag.
      [(? string? v)
       (values v v #f)]
      ;; Traced function calls:
      [_
       (define in-situ (string-join
                        (append (map ~v args)
                                (append-map list
                                            (map ~a kws)
                                            (map ~v kw-vals)))))
       (define show (~a "("
                        (syntax-e id)
                        (if (equal? in-situ "") "" " ")
                        in-situ
                        ")"))
       (values show in-situ -tail?)]))
  (with-tracing-mark
    (make-tracing-data #t tail? id show in-situ)
    (with-more-logging-info
      (log! (~a (make-string depth #\>) " " show)))))

(define-simple-macro (log-results e:expr ...)
  (when (log-level? logger level topic)
    (do-log-results e ...)))

(define (do-log-results id results depth)
  (define show
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  (with-tracing-mark
    (make-tracing-data #f #f id show show)
    (with-more-logging-info
     (log! (~a (make-string depth #\<) " " show)))))

(define (make-tracing-data call? tail? id show in-situ)
  (hasheq 'call          call?
          'tail          tail?
          'name          (~a (syntax-e id))
          'show          show
          'show-in-situ  in-situ
          'identifier    (->srcloc-as-list id)
          'formals       (get-formals-stx-prop id)
          'header        (get-header-stx-prop id)))

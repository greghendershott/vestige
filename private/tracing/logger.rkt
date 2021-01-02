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
         cms->tracing-data
         make-tracing-data)

;;; continuation mark

;; Intentionally not using make-continuation-mark-key.
(define key 'vestige-tracing-continuation-mark-key)

(define-simple-macro (with-tracing-mark data e:expr)
  (with-continuation-mark key data e))

(define (cms->tracing-data cms)
   (continuation-mark-set-first cms key))

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
  (define-values (message in-situ tail?)
    (match (expression-identifier->string id)
      ;; Traced expressions: Show the expression string. Disregard
      ;; caller loc and tail call flag.
      [(? string? v)
       (values v v #f)]
      ;; Traced function calls:
      [_
       (define args-str (string-join
                         (append (map ~v args)
                                 (append-map list
                                             (map ~a kws)
                                             (map ~v kw-vals)))))
       (define message (~a "("
                           (syntax-e id)
                           (if (equal? args-str "") "" " ")
                           args-str
                           ")"))
       (values message args-str -tail?)]))
  (with-tracing-mark (make-tracing-data #t tail? id message in-situ)
    (with-more-logging-info
      (log! (~a (make-string depth #\>) " " message)))))

(define-simple-macro (log-results e:expr ...)
  (when (log-level? logger level topic)
    (do-log-results e ...)))

(define (do-log-results id results depth)
  (define str
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  (with-tracing-mark (make-tracing-data #f #f id str str)
    (with-more-logging-info
     (log! (~a (make-string depth #\<) " " str)))))

(define (make-tracing-data call? tail? id message in-situ)
  (hasheq 'call          call?
          'tail          tail?
          'name          (~a (syntax-e id))
          'message       message
          'show-in-situ  in-situ
          'identifier    (->srcloc-as-list id)
          'formals       (get-formals-stx-prop id)
          'header        (get-header-stx-prop id)))

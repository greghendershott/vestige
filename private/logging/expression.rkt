#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/match
         syntax/parse/define
         (only-in "log.rkt" log? log!)
         "common.rkt")

(provide log-expression)

(define-syntax-parser log-expression
  [(_ e:expr)
   #:with new-e (syntax/loc #'e
                  (call-with-values
                   (λ () e)
                   (λ vs
                     (when (log?)
                       (log! (fmt 'e vs)))
                     (apply values vs))))
   (syntax/loc this-syntax
     (with-more-logging-info
       new-e))])

(define (fmt quoted-expr vs)
  (~a (~s quoted-expr)
      " => "
      (match vs
        [(list)   "#<void>"]
        [(list v) (~v v)]
        [vs       (~s (cons 'values vs))])))

(module+ example
  (log-expression (+ 1 2))
  (log-expression (string-append "foo" "bar"))
  (log-expression 'foo)
  (log-expression (values 1 2 3))
  (log-expression (void)))

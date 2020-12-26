#lang racket/base

(require (for-syntax racket/base
                     "srcloc.rkt")
         syntax/parse/define
         "srcloc.rkt")

(provide caller-key
         tracing-#%app
         caller-srcloc)

(define caller-key (make-continuation-mark-key 'caller))

(define-syntax-parser tracing-#%app
  [(_ x:expr more ...)
   (quasisyntax/loc this-syntax
     (with-continuation-mark caller-key '#(#,@(->srcloc-as-list this-syntax))
       (#%app x more ...)))])

(define (caller-srcloc cms)
  (define v (continuation-mark-set-first cms caller-key))
  (and v (->srcloc-as-list v)))

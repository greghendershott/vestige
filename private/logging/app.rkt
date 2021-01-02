#lang racket/base

(require (for-syntax racket/base
                     "srcloc.rkt")
         syntax/parse/define
         "srcloc.rkt")

(provide (rename-out [key app-key])
         tracing-#%app
         cms->caller-srcloc)

;; Intentionally not using make-continuation-mark-key because
;; vestige/reciving could be dynamic-required.
(define key 'vestige-app-srcloc-continuation-mark-key)

(define-syntax-parser tracing-#%app
  [(_ x:expr more ...)
   (quasisyntax/loc this-syntax
     (with-continuation-mark key '#(#,@(->srcloc-as-list this-syntax))
       (#%app x more ...)))])

(define (cms->caller-srcloc cms)
  (define v (continuation-mark-set-first cms key))
  (and v (->srcloc-as-list v)))

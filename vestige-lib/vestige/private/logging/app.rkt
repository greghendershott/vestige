#lang racket/base

(require (for-syntax racket/base
                     "srcloc.rkt")
         syntax/parse/define
         racket/match
         "srcloc.rkt")

(provide vestige-#%app
         cms->caller)

;; Intentionally not using make-continuation-mark-key.
(define key 'vestige-app-srcloc-continuation-mark-key)

(define-syntax-parser vestige-#%app
  [(_ proc-expr:expr more ...)
   (quasisyntax/loc this-syntax
     (let ([proc proc-expr])
       (with-continuation-mark key (list proc #,@(->srcloc-as-list this-syntax))
         (#%app proc-expr more ...))))])

(define (cms->caller cms proc)
  (match (continuation-mark-set-first cms key)
    [(cons actual-proc (? srcloc-as-list/c srcloc))
     (hasheq 'immediate (equal? actual-proc proc)
             'srcloc    srcloc)]
    [_ #f]))

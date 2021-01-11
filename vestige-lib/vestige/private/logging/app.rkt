#lang racket/base

(require (for-syntax racket/base
                     "srcloc.rkt")
         syntax/parse/define
         racket/match
         "srcloc.rkt")

(provide vestige-#%app
         immediate-caller-srcloc)

;; Intentionally not using make-continuation-mark-key.
(define key 'vestige-app-srcloc-continuation-mark-key)

(define-syntax-parser vestige-#%app
  [(_ proc-expr:expr more ...)
   (quasisyntax/loc this-syntax
     (let ([proc proc-expr])
       (with-continuation-mark key (list proc #,@(->srcloc-as-list this-syntax))
         (#%app proc more ...))))])

(define (immediate-caller-srcloc desired-calling-proc)
  (match (continuation-mark-set-first (current-continuation-marks) key)
    [(cons actual-calling-proc srcloc)
     #:when (equal? actual-calling-proc desired-calling-proc)
     (->srcloc-as-list srcloc)]
    [_ #f]))

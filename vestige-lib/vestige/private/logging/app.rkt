#lang racket/base

(require (for-syntax racket/base
                     "srcloc.rkt")
         syntax/parse/define
         racket/match
         "srcloc.rkt")

(provide vestige-#%app
         cms->caller-vector
         caller-vector->hasheq)

;; Intentionally not using make-continuation-mark-key.
(define key 'vestige-app-srcloc-continuation-mark-key)

(define-syntax-parser vestige-#%app
  [(_ proc-expr:expr more ...)
   (quasisyntax/loc this-syntax
     (let ([proc proc-expr])
       (with-continuation-mark key (list proc #,@(->srcloc-as-list this-syntax))
         (#%app proc-expr more ...))))])

(define (cms->caller-vector proc)
  ;; Use #f to mean (current-continuation-marks) to enable shortcuts.
  (match (continuation-mark-set-first #f key)
    [(cons actual-proc (? srcloc-as-list/c srcloc))
     (vector (equal? actual-proc proc) srcloc)]
    [_ #f]))

(define (caller-vector->hasheq v)
  (match v
    [(vector immediate? srcloc)
     (hasheq 'immediate immediate?
             'srcloc    srcloc)]
    [#f #f]))

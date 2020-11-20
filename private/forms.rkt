#lang racket/base

(require (for-syntax racket/base
                     syntax/define
                     syntax/name
                     syntax/parse
                     syntax/parse/lib/function-header
                     "expression-id.rkt")
         "core.rkt")

;; NOTE: These surface macros are fairly different from racket/trace.
;; We don't support mutating definitions with `trace` and `untrace`.
;; Instead we define things as traced in the first place. The core
;; form here is trace-lambda.

(provide trace-lambda
         (rename-out [trace-lambda trace-λ])
         trace-define
         trace-let
         trace-expression)

(define-syntax (trace-lambda stx)
  (define (infer-name-or-error)
    (or (syntax-local-infer-name stx)
        (raise-syntax-error
         'trace-lambda
         "Could not infer name; give a name explicitly using #:name"
         stx)))
  (syntax-parse stx
    [(_ (~optional (~seq #:name ID:id)
                   #:defaults ([ID (datum->syntax stx (infer-name-or-error) stx)]))
        ARGS:formals BODY:expr ...)
     (syntax/loc stx
       (wrap-with-tracing (λ ARGS BODY ...) #'ID))]))

(define-syntax (trace-define stx)
  (define-values (name def) (normalize-definition stx #'trace-lambda #t #t))
  (quasisyntax/loc stx (define #,name #,def)))

(define-syntax (trace-let stx)
  (syntax-parse stx
    ;; "Named `let`"
    [(_ name:id ([id:id e:expr] ...) body ...+)
     #`(let ()
         (trace-define (name id ...) body ...)
         ;; Ensure initial call gets good call-site srcloc by invoking
         ;; our tracing-#%app directly, otherwise it can be a bizarre
         ;; value. Futhermore, nice if the srloc is for #'name
         ;; specifically not the entire named-let stx.
         #,(syntax/loc #'name
             (tracing-#%app name e ...)))]
    ;; Normal `let`
    [_ stx]))

;; Note: Although it might seem silly to handle this with trace-lambda,
;; as opposed to simply logging the expression source and value
;; directly, the advantage is that the level will be correct. As a
;; result if some tool is indenting and/or folding by level, this will
;; appear naturally in relation to other trace-x forms -- including
;; nested uses of trace-expression. We use expression->identifier to
;; synthesize an identifier with a syntax property holding the datum
;; of #'e for use when logging.
(define-syntax (trace-expression stx)
  (syntax-parse stx
    [(_ e:expr)
     (with-syntax ([id (expression->identifier #'e)])
       (syntax/loc stx
         ((trace-lambda #:name id () e))))]))

(module+ test
  (require racket/logging
           racket/match
           rackunit
           "logger.rkt")
  (with-intercepted-logging
    (match-lambda [(vector _level
                           _message
                           (hash-table ['call call?] ['name name] ['show show])
                           _topic)
                   (check-equal? name "(+ 1 2)")
                   (check-equal? show (if call? "(+ 1 2)" "3"))])
    (λ ()
      (trace-expression (+ 1 2)))
    #:logger logger level topic))


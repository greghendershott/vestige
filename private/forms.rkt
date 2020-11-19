#lang racket/base

(require (for-syntax racket/base
                     syntax/define
                     syntax/name
                     syntax/parse
                     syntax/parse/lib/function-header
                     "expression-id.rkt")
         "core.rkt")

;; NOTE: These surface macros are fairly different from racket/trace.
;; We don't support the mutating `trace` and `untrace`, and so we
;; don't handle everything with that.

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
       (let*-values ([(proc) (λ ARGS BODY ...)]
                     [(arity) (procedure-arity proc)]
                     [(required allowed) (procedure-keywords proc)]
                     [(kw-proc) (λ (kws vals . args)
                                  (apply-traced #'ID proc args kws vals))]
                     [(plain-proc) (λ args
                                     (apply-traced #'ID proc args null null))]
                     [(traced-proc) (make-keyword-procedure kw-proc plain-proc)])
         (procedure-rename (procedure-reduce-keyword-arity traced-proc
                                                           arity
                                                           required
                                                           allowed)
                           'ID)))]))

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

;; Note: Although it might seem silly to handle this with trace-let,
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
       (syntax/loc stx (trace-let id () e)))]))

(module+ m
  (require racket/logging
           racket/pretty
           "logger.rkt")
  (with-intercepted-logging pretty-print
    (λ ()
      (define f (trace-lambda (#:x x y) (+ x y)))
      (f #:x 1 2)
      (trace-define (g #:x x y) (+ x y))
      (g #:x 4 5))
    #:logger logger level topic))


#lang racket/base

(require (for-syntax racket/base
                     syntax/define
                     syntax/name
                     syntax/parse
                     syntax/parse/lib/function-header
                     "expression-id.rkt"
                     "signature.rkt")
         "core.rkt")

;; NOTE: These surface macros are fairly different from racket/trace.
;; We don't support mutating definitions with `trace` and `untrace`.
;; Instead we define things as traced in the first place. The core
;; form here is trace-lambda.
;;
;; Furthermore we supply a `trace-expression`.
;;
;; We use two syntax properties attached to the function name
;; identifier, for a couple special purposes:
;;
;; 1. For trace-expression, this captures the original expresssion
;; datum string, for later use when logging.
;;
;; 2. For all forms, a "signature" property says what portion of the
;; source should be used if a "step tracer" tool wants to show an
;; application at the definition site. This varies among forms, and
;; can be the srloc for multiple consecutive pieces of original
;; syntax, as with a named let.

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
    [(_ (~optional (~seq #:name id:id)
                   #:defaults ([id (datum->syntax stx (infer-name-or-error) stx)]))
        args:formals body:expr ...)
     (with-syntax ([id (if (get-signature-stx-prop #'id)
                           #'id ;keep existing
                           (add-signature-stx-prop #'id #'args))])
      (syntax/loc stx
        (wrap-with-tracing (λ args body ...) #'id)))]))

(define-syntax (trace-define stx)
  (syntax-parse stx
    [(_ header:function-header body:expr ...+)
     (quasisyntax/loc stx
       (define header.name
         (trace-lambda
           #:name #,(add-signature-stx-prop #'header.name #'header)
           header.args
           body ...)))]
    [_
     (define-values (name def) (normalize-definition stx #'lambda #t #t))
     (quasisyntax/loc stx (define #,name #,def))]))

(define-syntax (trace-let stx)
  (syntax-parse stx
    ;; "Named `let`"
    [(_ name:id (~and bindings ([id:id e:expr] ...)) body ...+)
     (with-syntax ([name (add-signature-stx-prop #'name #'name #'bindings)])
       (quasisyntax/loc stx
         (let ()
           (define name (trace-lambda #:name name (id ...) body ...))
           ;; Ensure initial call gets good call-site srcloc by invoking
           ;; our tracing-#%app directly, otherwise it can be a bizarre
           ;; value. Futhermore, want srcloc for #'name specifically not
           ;; the entire named-let stx.
           #,(syntax/loc #'name
               (tracing-#%app name e ...)))))]
    ;; Normal `let`
    [(_ e:expr ...+)
     (syntax/loc stx (let e ...))]))

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
     (with-syntax ([id (add-signature-stx-prop (expression->identifier #'e)
                                               #'e)])
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


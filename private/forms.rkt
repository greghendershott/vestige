#lang racket/base

(require (for-syntax racket/base
                     syntax/define
                     syntax/name
                     syntax/parse
                     "expression-id.rkt")
         "core.rkt")

(provide trace
         untrace
         trace-define
         trace-lambda (rename-out [trace-lambda trace-λ])
         trace-let
         trace-expression)

(define-syntax (trace stx)
  (syntax-parse stx
    [(_ id:id ...)
     #'(begin (trace-one-procedure id) ...)]))

(define-syntax (untrace stx)
  (syntax-parse stx
    [(_ id:id ...)
     #'(begin (when (traced-procedure? id)
                (set! id (traced-procedure-untraced id)))
              ...)]))

(define-syntax (trace-define stx)
  (let-values ([(name def) (normalize-definition stx #'lambda #t #t)])
    #`(begin #,(quasisyntax/loc stx (define #,name #,def))
             (trace #,name))))

(define-syntax (trace-let stx)
  (syntax-parse stx
    ;; "Named `let`"
    [(_ name:id ([id:id e:expr] ...) body ...+)
     #`(let ()
         (trace-define (name id ...) body ...)
         #,(syntax/loc stx (name e ...)))]
    #;
    [(_ name:id ([id:id e:expr] ...) body ...+)
     #`(#,(quasisyntax/loc stx
            (letrec ([name (lambda (id ...) body ...)])
              (trace name)
              name))
        e ...)]
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

(define-syntax (trace-lambda stx)
  (define (infer-name-or-error)
    (or (syntax-local-infer-name stx)
        (raise-syntax-error
         'trace-lambda
         "Could not infer name; give a name explicitly using #:name"
         stx)))
  (syntax-parse stx
    [(_ (~optional (~seq #:name name:id)
                   #:defaults ([name (datum->syntax stx (infer-name-or-error) stx)]))
        args body:expr ...)
     #`(let ([name #,(quasisyntax/loc stx (lambda args body ...))]) (trace name) name)]))

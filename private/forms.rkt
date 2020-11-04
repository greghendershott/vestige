#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     racket/syntax
                     syntax/define
                     syntax/name
                     syntax/parse)
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
  (syntax-case stx ()
    [(_ e ...)
     (let-values ([(name def) (normalize-definition stx #'lambda #t #t)])
       #`(begin #,(quasisyntax/loc stx (define #,name #,def)) (trace #,name)))]))

(define-syntax trace-let
  (syntax-rules ()
    [(_ name ([x* e*] ...) body ...)
     ((letrec ([name (lambda (x* ...) body ...)]) (trace name) name)
      e* ...)]))

(define-syntax (trace-expression stx)
  (syntax-parse stx
    [(_ e:expr)
     (with-syntax ([id (syntax-property
                        (format-id #'e (~a (syntax->datum #'e)) #:source #'e)
                        'vestige-expression
                        (~a (syntax->datum #'e))
                        #t)])
       (quasisyntax/loc stx (trace-let id () e)))]))

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



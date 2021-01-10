#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/syntax format-id)
                     syntax/parse/lib/function-header
                     "infer-name.rkt" ;not syntax/name
                     "srcloc.rkt")
         syntax/parse/define
         "../logging/app.rkt"
         "wrap.rkt")

;; NOTE: These surface macros are fairly different from racket/trace.
;; We don't support mutating definitions with `trace` and `untrace`.
;; Instead we define things as traced in the first place. The core
;; form here is trace-lambda.

(provide trace-lambda
         (rename-out [trace-lambda trace-λ])
         trace-case-lambda
         trace-define
         trace-let)

(begin-for-syntax
  (define (infer-name-or-error stx who)
    (or (syntax-local-infer-name stx)
        (raise-syntax-error who
                            "Could not infer name; give a name explicitly using #:name"
                            stx)))

  (define (inferred-name-id stx who)
    (datum->syntax stx (infer-name-or-error stx who) stx)))

(define-syntax-parser do-trace-lambda
  [(_ {~seq #:name name}
      {~seq #:formals formals:formals}
      {~optional {~seq #:formals-stx-for-srcloc formals-srcloc-stx}
                 #:defaults ([formals-srcloc-stx #'formals])}
      {~optional {~seq #:header-stxs-for-srcloc [header-srcloc-stxs ...]}
                 #:defaults ([(header-srcloc-stxs 1) (list #'formals)])}
      body:expr ...+)
   #:with header-srcloc  (header-srcloc (syntax->list #'(header-srcloc-stxs ...)))
   #:with formals-srcloc (formals-srcloc #'formals-srcloc-stx)
   #:with positionals    (formals->positionals #'formals)
   ;; Give the lambda expression the srcloc from this-syntax so that
   ;; e.g. check-syntax tail reporting points to user's source not
   ;; here. (Macros below that expand to us, should also ensure that
   ;; our use has srcloc from the user's program, using syntax/loc
   ;; when necessary.)
   #:with proc           (syntax/loc this-syntax (lambda formals body ...))
   #'(chaperone-procedure
      (procedure-rename proc 'name)
      (make-chaperone-wrapper-proc 'name
                                   'header-srcloc
                                   'formals-srcloc
                                   'positionals)
      chaperone-prop-key
      chaperone-prop-val)])

(define-syntax-parser trace-lambda
  [(_ formals:formals body:expr ...+)
   #:with name (inferred-name-id this-syntax 'trace-lambda)
   #'(trace-lambda #:name name formals body ...)]
  [(_ {~seq #:name name:id} formals:formals body:expr ...+)
   #'(do-trace-lambda #:name name
                      #:formals formals
                      body ...)])

(define-syntax (trace-case-lambda stx)
  (define counts null)
  (define-syntax-class clause
    #:attributes (count name formals (body 1))
    (pattern ({~and formals (_:id ...)} body:expr ...+)
             #:do[(define n (length (syntax->list #'formals)))]
             #:fail-when (memq n counts) (format "duplicate clause for ~a arguments" n)
             #:do [(set! counts (cons n counts))]
             #:with name (format-id this-syntax "clause-~a" n)
             #:with count n))
  (syntax-parse stx
    [(_ clause:clause ...+)
     #:with name (inferred-name-id stx 'trace-case-lambda)
     #'(lambda args
         (let ([clause.name (trace-lambda #:name name
                                          clause.formals
                                          clause.body ...)] ...)
          (case (length args)
            [(clause.count) (apply clause.name args)] ...
            [else (apply raise-arity-error 'name '(clause.count ...) args)])))]))

(define-syntax-parser trace-define
  [(_ header:function-header body:expr ...+)
   #`(define header.name
       #,(let produce-lambda ([header #'header])
           (syntax-parse header
             [(name:id . fmls:formals)
              (quasisyntax/loc this-syntax
                (trace-lambda #:name name
                              fmls
                              body ...))]
             [(more . fmls:formals)
              #:with name #'header.name
              (quasisyntax/loc #'fmls
                (trace-lambda #:name name
                              fmls
                              #,(produce-lambda #'more)))])))]
  [(_ id:id expr:expr)
   (quasisyntax/loc this-syntax (define id expr))])

(define-syntax-parser trace-let
  ;; "Named let"
  [(_ name:id {~and bindings ([param:id init:expr] ...)} body ...+)
   (quasisyntax/loc this-syntax
     (letrec ([name #,(syntax/loc this-syntax
                        (do-trace-lambda #:name name
                                         #:formals (param ...)
                                         #:formals-stx-for-srcloc bindings
                                         #:header-stxs-for-srcloc [id bindings]
                                         body ...))])
       #,(syntax/loc #'name ;good srcloc for initial call
           (vestige-#%app name init ...))))]
  ;; Normal let
  [(_ e:expr ...+)
   (syntax/loc this-syntax (let e ...))])

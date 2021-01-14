#lang racket/base

(require (for-syntax racket/base
                     racket/match
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
  [(_ #:name       name
      #:formals    formals:formals
      #:definition defn-stx
      {~optional {~seq #:formals-stx-for-srcloc formals-srcloc-stx}
                 #:defaults ([formals-srcloc-stx #'formals])}
      {~optional {~seq #:header-stxs-for-srcloc [header-srcloc-stxs ...]}
                 #:defaults ([(header-srcloc-stxs 1) (list #'formals)])}
      body:expr    ...+)
   #:with defn-srcloc    (->srcloc-as-list #'defn-stx)
   #:with header-srcloc  (header-srcloc (syntax->list #'(header-srcloc-stxs ...)))
   #:with formals-srcloc (formals-srcloc #'formals-srcloc-stx)
   #:with positionals    (formals->positionals #'formals)
   ;; Give the lambda expression the srcloc from this-syntax so that
   ;; e.g. check-syntax tail reporting points to user's source not
   ;; here. (Macros below that expand to us, should also ensure that
   ;; our use has srcloc from the user's program, using syntax/loc
   ;; when necessary.)
   #:with lam            (syntax/loc this-syntax (lambda formals body ...))
   #'(let ([proc (procedure-rename lam 'name)])
       (make-wrapper-proc proc
                          'name
                          'defn-srcloc
                          'header-srcloc
                          'formals-srcloc
                          'positionals))])

(define-syntax-parser trace-lambda
  [(_ formals:formals body:expr ...+)
   #:with name (inferred-name-id this-syntax 'trace-lambda)
   (syntax/loc this-syntax
     (trace-lambda #:name name formals body ...))]
  [(_ {~seq #:name name:id} formals:formals body:expr ...+)
   (quasisyntax/loc this-syntax
     (do-trace-lambda #:name       name
                      #:formals    formals
                      #:definition #,this-syntax
                      body ...))])

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
     (quasisyntax/loc this-syntax
       (lambda args
         (let ([clause.name (do-trace-lambda #:name       name
                                             #:formals    clause.formals
                                             #:definition #,this-syntax
                                             clause.body ...)]
               ...)
           (case (length args)
             [(clause.count) (apply clause.name args)]
             ...
             [else (apply raise-arity-error
                          'name
                          '(clause.count ...)
                          args)]))))]))

(define-syntax-parser trace-define
  [(_ id:id expr:expr)
   (quasisyntax/loc this-syntax (define id expr))]
  [(_ header:function-header body:expr ...+)
   ;; (define ((foo x) y) (list x y))
   ;; =>
   ;; (define foo (λ (x) (λ (y) (list x y))))
   (define all-formals
     (reverse
      (let loop ([header #'header])
        (syntax-parse header
          [(_:id . fs:formals) (list #'fs)]
          [(more . fs:formals) (cons #'fs (loop #'more))]))))
   (define overall-this-syntax this-syntax)
   #`(define header.name
       #,(let produce-lambda ([all-formals all-formals])
           (match all-formals
             [(list fmls)
              (syntax-property
               (quasisyntax/loc this-syntax
                 (do-trace-lambda #:name       header.name
                                  #:formals    #,fmls
                                  #:definition #,overall-this-syntax
                                  body ...))
               'inferred-name #'header.name)]
             [(cons fmls more)
              (syntax-property
               (quasisyntax/loc this-syntax
                 (do-trace-lambda #:name       header.name
                                  #:formals    #,fmls
                                  #:definition #,overall-this-syntax
                                  #,(produce-lambda more)))
               'inferred-name #'header.name)])))])

(define-syntax-parser trace-let
  ;; "Named let"
  [(_ name:id {~and bindings ([param:id init:expr] ...)} body ...+)
   (quasisyntax/loc this-syntax
     (letrec ([name #,(quasisyntax/loc this-syntax
                        (do-trace-lambda #:name                   name
                                         #:formals                (param ...)
                                         #:definition             #,this-syntax
                                         #:formals-stx-for-srcloc bindings
                                         #:header-stxs-for-srcloc [name bindings]
                                         body ...))])
       #,(syntax/loc #'name ;good srcloc for initial call
           (vestige-#%app name init ...))))]
  ;; Normal let
  [(_ e:expr ...+)
   (syntax/loc this-syntax (let e ...))])

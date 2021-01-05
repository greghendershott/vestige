#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     (only-in racket/string string-join)
                     (only-in racket/syntax format-id)
                     (only-in syntax/define normalize-definition)
                     (only-in syntax/name syntax-local-infer-name)
                     syntax/parse/lib/function-header
                     "id-stx-prop.rkt")
         syntax/parse/define
         "../logging/app.rkt"
         "core.rkt")

;; NOTE: These surface macros are fairly different from racket/trace.
;; We don't support mutating definitions with `trace` and `untrace`.
;; Instead we define things as traced in the first place. The core
;; form here is trace-lambda.
;;
;; We use syntax properties attached to the function name identifier:
;; "formals" and "header" syntax properties say what portion of the
;; source should be used if a "step tracer" tool wants to show actual
;; arguments at the definition site, an application at the caller
;; site, or results at both sites. These source spans vary among
;; forms, and can be the srloc for multiple consecutive pieces of
;; original syntax, as with a named let. See add-prop.

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

(define-syntax-parser trace-lambda
  [(_ (~optional (~seq #:name name:id)
                 #:defaults ([name (inferred-name-id this-syntax 'trace-lambda)]))
      formals:formals body:expr ...+)
   ;; `make-chaperone-wrapper-proc` takes a piece of identifier
   ;; syntax: it uses the symbol value in messages and it calls
   ;; get-prop to get additional information. When #:name is supplied
   ;; we use that. Otherwise we fall back to making our own identifier
   ;; from the inferred name symbol. In any case, if get-prop returns
   ;; #f for the name identifier, we do an add-prop here.
   #:with name+props (if (get-prop #'name)
                         #'name ;keep existing properties
                         (add-prop #'name
                                   #:formals-stx #'formals
                                   #:header-stxs (list #'formals)))
   ;; Give the lambda srcloc from this-syntax so that e.g.
   ;; check-syntax tail reporting points to user's source not here.
   ;; (Macros below that expand to us, should also ensure that our use
   ;; has srcloc from the user's program, using syntax/loc when
   ;; necessary.)
   #:with proc (syntax/loc this-syntax (lambda formals body ...))
   #`(chaperone-procedure (procedure-rename proc 'name)
                          (make-chaperone-wrapper-proc #'name+props)
                          chaperone-prop-key
                          chaperone-prop-val)])

(define-syntax (trace-case-lambda stx)
  (define name-sym (infer-name-or-error stx 'trace-case-lambda))
  (define-syntax-class clause
    #:attributes (num-args trace-lambda)
    (pattern (formals:formals body:expr ...+)
             #:with num-args (length (syntax->list #'formals))
             #:with name (add-prop (format-id #'formals "~a" name-sym)
                                   #:formals-stx #'formals
                                   #:header-stxs (list #'formals))
             #:with trace-lambda (syntax/loc stx
                                   (trace-lambda #:name name formals body ...))))
  (syntax-parse stx
    [(_ clause:clause ...+)
     #:with name name-sym
     #'(lambda args
         (case (length args)
           [(clause.num-args) (apply clause.trace-lambda args)] ...
           [else (apply raise-arity-error 'name '(clause.num-args ...) args)]))]))

(define-syntax-parser trace-define
  [(_ header:function-header body:expr ...+)
   ;; Flatten/reverse the formals to be able to generate the nested
   ;; lambdas.
   (define all-formals
     (reverse
      (let loop ([header #'header])
        (syntax-parse header
          [(_:id . fs:formals)
           (list #'fs)]
          [(more . fs:formals)
           (cons #'fs (loop #'more))]))))
   (define curried? (not (null? (cdr all-formals))))
   ;; For e.g. (define ((f x0 x1) y0 y1) _) synthesize trace-lambda
   ;; #:name identifiers like "foo{x0 x1}" and "foo{y0 y1}",
   ;; attaching a formals stx prop for the specific nested formals'
   ;; srcloc.
   (define (name-id fs)
     (define params (if curried? (formals->curly-params fs) ""))
     (format-id #f "~a~a" #'header.name params #:source fs))
   #`(define header.name
       #,(let produce-lambda ([all-formals all-formals])
           (match all-formals
             [(list fmls)
              (quasisyntax/loc this-syntax
                (trace-lambda #:name #,(name-id fmls) #,fmls
                              body ...))]
             [(cons fmls more)
              (quasisyntax/loc fmls
                (trace-lambda #:name #,(name-id fmls) #,fmls
                              #,(produce-lambda more)))])))]
  [_
   (define-values (name def) (normalize-definition this-syntax #'lambda #t #t))
   (quasisyntax/loc this-syntax (define #,name #,def))])

(begin-for-syntax
  (define (formals->curly-params fmls)
    (syntax-parse fmls
      [(f:formal ...)
       (string-append
        "{" (string-join (map (compose1 symbol->string syntax-e)
                              (syntax->list #'(f.name ...)))) "}")])))

(define-syntax-parser trace-let
  ;; "Named let"
  [(_ id:id (~and bindings ([param:id init:expr] ...)) body ...+)
   #:with name (add-prop #'id
                         #:formals-stx #'bindings
                         #:header-stxs (list #'id #'bindings))
   (quasisyntax/loc this-syntax
     (letrec ([name #,(syntax/loc this-syntax
                        (trace-lambda #:name name (param ...) body ...))])
       #,(syntax/loc #'name ;good srcloc for initial call
           (vestige-#%app name init ...))))]
  ;; Normal let
  [(_ e:expr ...+)
   (syntax/loc this-syntax (let e ...))])


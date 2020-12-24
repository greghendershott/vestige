#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     (only-in racket/string string-join)
                     (only-in racket/syntax format-id)
                     (only-in syntax/define normalize-definition)
                     (only-in syntax/name syntax-local-infer-name)
                     syntax/parse/lib/function-header
                     "expression-id.rkt"
                     "stxprops.rkt")
         syntax/parse/define
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
;; datum string, for later use when logging. See expresion->id.
;;
;; 2. For all forms, "formals" and "header" syntax properties say
;; what portion of the source should be used if a "step tracer" tool
;; wants to show an application at the definition site. This varies
;; among forms, and can be the srloc for multiple consecutive pieces
;; of original syntax, as with a named let. See add-stx-props.

(provide trace-lambda
         (rename-out [trace-lambda trace-λ])
         trace-case-lambda
         trace-define
         trace-let
         trace-expression)

(begin-for-syntax
  (define (infer-name-or-error stx who)
    (or (syntax-local-infer-name stx)
        (raise-syntax-error who
                            "Could not infer name; give a name explicitly using #:name"
                            stx)))

  (define (inferred-name-id stx who)
    (datum->syntax stx (infer-name-or-error stx who) stx)))

(define-syntax-parser trace-lambda
  [(_ (~optional (~seq #:name NAME:id)
                 #:defaults ([NAME (inferred-name-id this-syntax 'trace-lambda)]))
      FORMALS:formals BODY:expr ...+)
   ;; `make-chaperone-wrapper-proc` takes a piece of identifier
   ;; syntax: it uses the symbol value in messages and it looks for
   ;; some syntax properties. When #:name is supplied we use that.
   ;; Otherwise we fall back to making our own identifier from the
   ;; inferred name symbol. In any case, if the name identifier
   ;; lacks a formals syntax property, we give it one corresponding
   ;; to the formals syntax, as well as a header property.
   #:with NAME+PROPS (if (get-formals-stx-prop #'NAME)
                         #'NAME ;keep existing properties
                         (add-stx-props #'NAME
                                        #:formals-stx #'FORMALS
                                        #:header-stxs (list #'FORMALS)))
   ;; Give the lambda srcloc from this-syntax so that e.g.
   ;; check-syntax tail reporting points to user's source not here.
   ;; (Macros below that expand to us, should also ensure that our use
   ;; has srcloc from the user's program, using syntax/loc when
   ;; necessary.)
   #:with PROC (syntax/loc this-syntax (lambda FORMALS BODY ...))
   #`(chaperone-procedure PROC
                          (make-chaperone-wrapper-proc #'NAME+PROPS)
                          chaperone-prop-key
                          chaperone-prop-val)])

(define-syntax (trace-case-lambda stx)
  (define name-sym (infer-name-or-error stx 'trace-case-lambda))
  (define-syntax-class clause
    #:attributes (num-args trace-lambda)
    (pattern (FORMALS:formals BODY:expr ...+)
             #:with num-args (length (syntax->list #'FORMALS))
             #:with name (add-stx-props (format-id #'FORMALS "~a" name-sym)
                                        #:formals-stx #'FORMALS
                                        #:header-stxs (list #'FORMALS))
             #:with trace-lambda (syntax/loc stx
                                   (trace-lambda #:name name FORMALS BODY ...))))
  (syntax-parse stx
    [(_ CLAUSE:clause ...+)
     #:with NAME name-sym
     #'(lambda args
         (case (length args)
           [(CLAUSE.num-args) (apply CLAUSE.trace-lambda args)] ...
           [else (apply raise-arity-error 'NAME '(CLAUSE.num-args ...) args)]))]))

(define-syntax-parser trace-define
  [(_ HEADER:function-header BODY:expr ...+)
   ;; Flatten/reverse the formals to be able to generate the nested
   ;; lambdas.
   (define all-formals
     (reverse
      (let loop ([header #'HEADER])
        (syntax-parse header
          [(_:id . FORMALS:formals)
           (list #'FORMALS)]
          [(MORE . FORMALS:formals)
           (cons #'FORMALS (loop #'MORE))]))))
   (define curried? (not (null? (cdr all-formals))))
   ;; For e.g. (define ((f x0 x1) y0 y1) _) synthesize trace-lambda
   ;; #:name identifiers like "foo{x0 x1}" and "foo{y0 y1}",
   ;; attaching a formals stx prop for the specific nested formals'
   ;; srcloc.
   (define (name-id fs)
     (define params (if curried? (formals->curly-params fs) ""))
     (format-id #f "~a~a" #'HEADER.name params #:source fs))
   #`(define HEADER.name
       #,(let produce-lambda ([all-formals all-formals])
           (match all-formals
             [(list fmls)
              (quasisyntax/loc this-syntax
                (trace-lambda #:name #,(name-id fmls) #,fmls
                              BODY ...))]
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
  ;; "Named `let`".
  [(_ ID:id (~and BINDINGS ([PARAM:id E:expr] ...)) BODY ...+)
   #:with NAME (add-stx-props #'ID
                              #:formals-stx #'BINDINGS
                              #:header-stxs (list #'ID #'BINDINGS))
   (quasisyntax/loc this-syntax
     (letrec ([NAME #,(syntax/loc this-syntax
                        (trace-lambda #:name NAME (PARAM ...) BODY ...))])
       #,(syntax/loc #'NAME ;good srcloc for initial call
           (tracing-#%app NAME E ...))))]
  ;; Normal `let`
  [(_ E:expr ...+)
   (syntax/loc this-syntax (let E ...))])

;; Note: Although it might seem silly to handle this with
;; trace-lambda, as opposed to simply logging the expression source
;; and value directly, one advantage is that the level will be
;; correct. As a result if some tool is indenting and/or folding by
;; level, this will appear naturally in relation to other trace-x
;; forms -- including nested uses of trace-expression. We use
;; expression->identifier to synthesize an identifier with a syntax
;; property holding the datum of #'e for use when logging.
(define-syntax-parser trace-expression
  [(_ E:expr)
   #:with NAME (add-stx-props/expression (expression->identifier #'E) #'E)
   (syntax/loc this-syntax
     ((trace-lambda #:name NAME () E)))])

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
    (λ () (trace-expression (+ 1 2)))
    #:logger logger level topic))

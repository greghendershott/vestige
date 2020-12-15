#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     racket/string
                     (only-in racket/syntax format-id)
                     syntax/define
                     syntax/name
                     syntax/parse
                     syntax/parse/lib/function-header
                     "expression-id.rkt"
                     "stxprops.rkt")
         (for-meta 2 racket/base)
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
;; 2. For all forms, a "formals" property says what portion of the
;; source should be used if a "step tracer" tool wants to show an
;; application at the definition site. This varies among forms, and
;; can be the srloc for multiple consecutive pieces of original
;; syntax, as with a named let.

(provide trace-lambda
         (rename-out [trace-lambda trace-λ])
         trace-case-lambda
         trace-define
         trace-let
         trace-expression)

(define-syntax (trace-lambda stx)
  ;; `wrap-with-tracing` takes a piece of identifier syntax. It uses
  ;; the symbol value in messages. It also looks for some syntax
  ;; properties. When #:name is supplied we use that. Otherwise we
  ;; fall back to making our own identifier from the inferred name
  ;; symbol. In any case, if the name identifier lacks a formals
  ;; syntax property, we give it one corresponding to the formals
  ;; syntax.
  (define (infer-name-or-error)
    (or (syntax-local-infer-name stx)
        (raise-syntax-error
         'trace-lambda
         "Could not infer name; give a name explicitly using #:name"
         stx)))
  (syntax-parse stx
    [(_ (~optional (~seq #:name ID:id)
                   #:defaults ([ID (datum->syntax stx (infer-name-or-error) stx)]))
        ARGS:formals BODY:expr ...+)
     (with-syntax ([ID (if (get-formals-stx-prop #'ID)
                           #'ID ;keep existing
                           (add-stx-props #'ID
                                          #:formals-stx #'ARGS
                                          #:header-stxs (list #'ARGS)))])
       (syntax/loc stx
         (wrap-with-tracing (λ ARGS BODY ...) #'ID)))]))

(define-syntax (trace-case-lambda stx)
  (define inferred-name
    (or (syntax-local-infer-name stx)
        (raise-syntax-error 'trace-case-lambda "Could not infer name" stx)))
  (define-syntax-class clause
    (pattern (formals:formals body:expr ...+)
             #:with len  (length (syntax->list #'formals))
             #:with name (add-stx-props (format-id #'formals "~a" inferred-name)
                                        #:formals-stx #'formals
                                        #:header-stxs (list #'formals))))
  (syntax-parse stx
    [(_ CLAUSE:clause ...+)
     #:with INFERRED-NAME inferred-name
     (quasisyntax/loc stx
       (lambda args
         (case (length args)
           [(CLAUSE.len) (apply (trace-lambda #:name CLAUSE.name
                                              CLAUSE.formals
                                              CLAUSE.body ...)
                                args)] ...
           [else (apply raise-arity-error
                        'INFERRED-NAME
                        '(CLAUSE.len ...)
                        args)])))]))

(define-syntax (trace-define stx)
  (syntax-parse stx
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
     (quasisyntax/loc stx
       (define HEADER.name
         #,(let loop ([fss all-formals])
             (match fss
               [(list fs)
                (quasisyntax/loc fs
                  (trace-lambda #:name #,(name-id fs) #,fs
                                BODY ...))]
               [(cons fs more)
                (quasisyntax/loc fs
                  (trace-lambda #:name #,(name-id fs) #,fs
                                #,(loop more)))]))))]
    [_
     (define-values (name def) (normalize-definition stx #'lambda #t #t))
     (quasisyntax/loc stx (define #,name #,def))]))

(begin-for-syntax
  (define (formals->curly-params fs)
    (syntax-parse fs
      [(f:formal ...)
       (string-append
        "{" (string-join (map (compose1 symbol->string syntax-e)
                              (syntax->list #'(f.name ...)))) "}")])))

(define-syntax (trace-let stx)
  (syntax-parse stx
    ;; "Named `let`".
    [(_ NAME:id (~and BINDINGS ([ID:id E:expr] ...)) BODY ...+)
     (with-syntax ([NAME (add-stx-props #'NAME
                                        #:formals-stx #'BINDINGS
                                        #:header-stxs (list #'NAME #'BINDINGS))])
       (quasisyntax/loc stx
         (letrec ([NAME (trace-lambda #:name NAME (ID ...) BODY ...)])
           #,(syntax/loc #'NAME ;good srcloc for initial call
               (tracing-#%app NAME E ...)))))]
    ;; Normal `let`
    [(_ E:expr ...+)
     (syntax/loc stx (let E ...))]))

;; Note: Although it might seem silly to handle this with
;; trace-lambda, as opposed to simply logging the expression source
;; and value directly, one advantage is that the level will be
;; correct. As a result if some tool is indenting and/or folding by
;; level, this will appear naturally in relation to other trace-x
;; forms -- including nested uses of trace-expression. We use
;; expression->identifier to synthesize an identifier with a syntax
;; property holding the datum of #'e for use when logging.
(define-syntax (trace-expression stx)
  (syntax-parse stx
    [(_ EXPR:expr)
     (with-syntax ([id (add-stx-props/expression (expression->identifier #'EXPR)
                                                 #'EXPR)])
       (syntax/loc stx
         ((trace-lambda #:name id () EXPR))))]))

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

#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/lib/function-header
                     "srcloc.rkt")
         racket/class
         syntax/parse/define
         "wrap.rkt")

;; A macro-defining macro. Given a racket/class keyword like
;; `private`, define an alternative to `define/private`:
;; `trace-define/private`.
(define-syntax-parser define/provide-method-definer
  [(_ class-keyword:id)
   #:with definer-name (format-id #'class-keyword
                                  "trace-define/~a"
                                  (syntax-e #'class-keyword)
                                  #:source #'class-keyword)
   (syntax/loc this-syntax
     (begin
       (define-syntax-parser definer-name
         [(_ id:id expr:expr)
          (syntax/loc this-syntax
            (begin
              (class-keyword id)
              (define id expr)))]
         [(_ {~and header (id:id . formals:formals)}
             body:expr ...+)
          #:with defn-srcloc    (->srcloc-as-list this-syntax)
          #:with header-srcloc  (header-srcloc (syntax->list #'(header)))
          #:with formals-srcloc (formals-srcloc #'formals)
          #:with positionals    (cons 'self (formals->positionals #'formals))
          (syntax/loc this-syntax
            (begin
              (class-keyword id)
              ;; Here racket/class need us to expand to something
              ;; matching its method-definition grammar:
              ;; (define-values (id) <method-proceure>). For
              ;; <method-procedure> fortunately one choice is
              ;; chaperone-procedure.
              (define-values (id)
                (chaperone-procedure
                 (lambda formals body (... ...))
                 (make-chaperone-wrapper-proc void
                                              'id
                                              'defn-srcloc
                                              'header-srcloc
                                              'formals-srcloc
                                              'positionals)
                 chaperone-prop-key
                 chaperone-prop-val))))])
       (provide definer-name)))])

(define/provide-method-definer private)
(define/provide-method-definer public)
(define/provide-method-definer pubment)
(define/provide-method-definer override)
(define/provide-method-definer overment)
(define/provide-method-definer augride)
(define/provide-method-definer augment)
(define/provide-method-definer public-final)
(define/provide-method-definer override-final)
(define/provide-method-definer augment-final)


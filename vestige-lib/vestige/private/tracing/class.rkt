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
          #:with defn-srcloc     (->srcloc-as-list this-syntax)
          #:with header-srcloc   (header-srcloc (syntax->list #'(header)))
          #:with formals-srcloc  (formals-srcloc #'formals)
          #:with positionals     (formals->positionals #'formals)
          #:with wrapper-id      (format-id #'id "~a.~a" #'id (gensym 'wrapper.))
          #:with (arg (... ...)) (formals->actuals #'formals)
          (syntax/loc this-syntax
            (begin
              (class-keyword id)
              ;; The `class` macro partially expands and looks for a
              ;; restricted grammar of shapes. As a result we can't
              ;; simply do something like:
              ;;
              ;; (define-values (id)
              ;;   (make-wrapper-proc (lambda formals body ...) ___))
              ;;
              ;; So, instead we define a wrapper as a plain function
              ;; definition...
              (define wrapper-id
                (make-wrapper-proc (lambda formals body (... ...))
                                   'id
                                   'defn-srcloc
                                   'header-srcloc
                                   'formals-srcloc
                                   'positionals))
              ;; ... then define the method using one of the
              ;; acceptable shapes. The method calls the wrapper. (Of
              ;; course that means "wrapper" is a misnomer; this could
              ;; work differently, but I'm following the model of
              ;; trace-define-lambda.)
              (define-values (id)
                (lambda formals
                  (wrapper-id arg (... ...))))))])
       (provide definer-name)))])

(begin-for-syntax
  ;; I split this out as a helper only because I was getting confusing
  ;; messages about ellipses, even being careful to use `(... ...)`
  ;; instead of `...` when applicable (or so I thought).
  (define (formals->actuals stx)
    (syntax-parse (syntax->list stx)
      [(f:formal ...)
       #'((~@ (~? f.kw) f.name) ...)])))

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


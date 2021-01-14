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
          #:with positionals    (formals->positionals #'formals)
          (quasisyntax/loc this-syntax
            (begin
              (class-keyword id)
              (define-values (id)
                (lambda formals
                  ;; Stupid, slow first version; calls
                  ;; make-wrapper-proc every time.
                  (let ([w (make-wrapper-proc (lambda formals body (... ...))
                                              'id
                                              'defn-srcloc
                                              'header-srcloc
                                              'formals-srcloc
                                              'positionals)])
                    (w #,@#'formals.params))))))])
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

(module+ example
  (define fish%
    (class object%
      (init size)                ; initialization argument
      (define current-size size) ; field
      (super-new)                ; superclass initialization
      (trace-define/public (get-size)
        current-size)
      (trace-define/public (grow amt)
                           (println this)
                           (set! current-size (+ amt current-size))
                           10)
      (trace-define/public (eat other-fish)
        (grow (send other-fish get-size)))))

  (define picky-fish%
    (class fish% (super-new)
      (trace-define/override (grow amt)
                             (super grow (* 3/4 amt)))))

  (define daisy (new picky-fish% [size 20]))
  (send daisy get-size)
  (send daisy grow 100)
  (send daisy get-size)

  (define charlie (new fish% [size 500]))
  (send daisy eat charlie))

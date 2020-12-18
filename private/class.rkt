#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/lib/function-header
                     "stxprops.rkt")
         racket/class
         "core.rkt")

(define-syntax (define/provide-method-definer stx)
  (syntax-parse stx
    [(_ KEYWORD)
     (with-syntax ([NAME (format-id #'KEYWORD
                                    "trace-define/~a"
                                    (syntax-e #'KEYWORD)
                                    #:source #'KEYWORD)])
       (syntax/loc stx
         (begin
          (define-syntax (NAME stx)
            (syntax-parse stx
              [(_ (~and HEADER (ID:id . FORMALS:formals)) BODY:expr ...+)
               (with-syntax ([ID (add-stx-props #'ID
                                                #:formals-stx #'FORMALS
                                                #:header-stxs (list #'HEADER))])
                 (syntax/loc stx
                   (begin
                     (KEYWORD ID)
                     (define-values (ID)
                       (chaperone-procedure (lambda FORMALS BODY (... ...))
                                            (make-chaperone-wrapper-proc #'ID)
                                            chaperone-prop-key
                                            chaperone-prop-val)))))]))
          (provide NAME))))]))

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
      (define/public (get-size)
        current-size)
      (trace-define/public (grow amt)
                           (set! current-size (+ amt current-size)))
      (define/public (eat other-fish)
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

#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/lib/function-header
                     "stxprops.rkt")
         racket/class
         (only-in racket/list append-map)
         "stxprops.rkt"
         "core.rkt")

(provide trace-define/public
         trace-define/override)

#;
(define-syntaxes (trace-define/public
                  trace-define/override)
  (let ([mk
         (λ (decl-form)
           (λ (stx)
             (syntax-parse stx
               [(_ (~and header (id:id . formals:formals)) body:expr ...+)
                (with-syntax ([id (add-stx-props #'id
                                                 #:formals-stx #'formals
                                                 #:header-stxs (list #'header))])
                  (quasisyntax/loc stx
                    (begin
                      (#,decl-form)
                      (define-values (id)
                        (chaperone-procedure (lambda formals body ...)
                                             (make-wrapper-proc #'id))))))])))])
    (values (mk #'public)
            (mk #'override))))

(define-syntax (trace-define/override stx)
  (syntax-parse stx
    [(_ (~and HEADER (ID:id . FORMALS:formals)) BODY:expr ...+)
     (with-syntax ([ID (add-stx-props #'ID
                                      #:formals-stx #'FORMALS
                                      #:header-stxs (list #'HEADER))])
       (syntax/loc stx
         (begin
           (override ID)
           (define-values (ID)
             (chaperone-procedure (lambda FORMALS BODY ...)
                                  (make-chaperone-wrapper-proc #'ID)
                                  chaperone-prop-key
                                  chaperone-prop-val)))))]))

(define-syntax (trace-define/public stx)
  (syntax-parse stx
    [(_ (~and HEADER (ID:id . FORMALS:formals)) BODY:expr ...+)
     (with-syntax ([ID (add-stx-props #'ID
                                      #:formals-stx #'FORMALS
                                      #:header-stxs (list #'HEADER))])
       (syntax/loc stx
         (begin
           (public ID)
           (define-values (ID)
             (chaperone-procedure (lambda FORMALS BODY ...)
                                  (make-chaperone-wrapper-proc #'ID)
                                  chaperone-prop-key
                                  chaperone-prop-val)))))]))

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

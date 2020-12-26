#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/lib/function-header
                     "loc-stx-props.rkt")
         racket/class
         syntax/parse/define
         "core.rkt")

;; A macro-defining macro. Given a racket/class keyword like
;; `private`, define an alternative to `define/private`:
;; `trace-define/private`.
(define-syntax-parser define/provide-method-definer
  [(_ class-keyword)
   #:with define-name (format-id #'class-keyword
                                 "trace-define/~a"
                                 (syntax-e #'class-keyword)
                                 #:source #'class-keyword)
   (syntax/loc this-syntax
     (begin
       (define-syntax-parser define-name
         [(_ (~and header (method-name:id . formals:formals)) body:expr ...+)
          #:with method-name+props (add-loc-props #'method-name
                                                  #:formals-stx #'formals
                                                  #:header-stxs (list #'header))
          (syntax/loc this-syntax
            (begin
              (class-keyword method-name+props)
              (define-values (method-name+props)
                (chaperone-procedure (lambda formals body (... ...))
                                     (make-chaperone-wrapper-proc #'method-name+props)
                                     chaperone-prop-key
                                     chaperone-prop-val))))])
       (provide define-name)))])

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


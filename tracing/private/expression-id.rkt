#lang racket/base

(require racket/syntax
         racket/format)

(provide expression->identifier
         expression-identifier->string)

(define property-key 'vestige-expression)

;; Given syntax for an arbitrary expression, generate an identifier.
;;
;; - The identifier's srcloc is that of the original expression.
;;
;; - A string of the expression datum e.g. "(+ 1 2)" is both:
;;
;;   - attached as a syntax property value for later retrieval
;;
;;   - used to format the identifier's symbol name.
;;
;; Although that means the symbol could be e.g. |(+ 1 2)| we don't
;; really care because in the intended use case that will be ignored
;; in favor of the property value.
;;
;; Although that means the symbol value will be identical for
;; identical expressions, we don't really care because the intended
;; use case is to create the identifer for a named let, so it will be
;; scoped by that.
(define (expression->identifier stx)
  (define as-str (~a (syntax->datum stx)))
  (define identifier (format-id stx as-str #:source stx))
  (syntax-property identifier
                   property-key
                   as-str
                   #t)) ;preserved

(define (expression-identifier->string stx)
  (syntax-property stx property-key))

(module+ test
  (require rackunit)
  (define e #'(+ 1 2))
  (define id (expression->identifier e))
  (check-equal? (syntax-e id) '|(+ 1 2)|
                "identifier has expected symbolic value")
  (check-equal? (expression-identifier->string id) "(+ 1 2)"
                "identifer has expected syntax property value")
  (for ([f (in-list (list syntax-source
                          syntax-line
                          syntax-column
                          syntax-position
                          syntax-span))])
    (check-equal? (f e) (f id)
                  (~a "expression and identifier syntax objects have equal "
                      (object-name f)))))
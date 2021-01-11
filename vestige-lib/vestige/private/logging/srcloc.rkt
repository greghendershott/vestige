#lang racket/base

(require racket/contract/base
         racket/format
         racket/match)

(provide ->srcloc-as-list
         srcloc-as-list/c)

(define srcloc-as-list/c
  (list/c (or/c #f (and/c string? path-string?))
          (or/c #f exact-positive-integer?)
          (or/c #f exact-nonnegative-integer?)
          (or/c #f exact-positive-integer?)
          (or/c #f exact-nonnegative-integer?)))

(define (->srcloc-as-list v)
  (match v
    [(or (list   (app clean-source src) line column position span)
         (srcloc (app clean-source src) line column position span))
     (list src line column position span)]
    [(? syntax? stx)
     (list (clean-source (syntax-source stx))
           (syntax-line stx)
           (syntax-column stx)
           (syntax-position stx)
           (syntax-span stx))]
    [_ (raise-argument-error '->srcloc-as-list
                             "syntax, srcloc, or srcloc as list"
                             v)]))

(define (clean-source source)
  (match source
    [(? path? p) (path->string p)]
    [(? path-string? s) s]
    [(? symbol? s) (~a s)]
    [(? procedure? (app object-name s)) #:when s (~a s)]
    [_ #f]))

(module+ test
  (require rackunit)
  (check-equal? (clean-source (build-path "foo")) "foo")
  (check-equal? (clean-source "foo") "foo")
  (check-equal? (clean-source 'foo) "foo")
  (check-equal? (clean-source clean-source) "clean-source")
  (check-false (clean-source 42)))

#lang racket/base

(require racket/format
         racket/match)

(provide ->srcloc-as-list)

(define (->srcloc-as-list v)
  (match v
    [(or (vector (app clean-source src) line column position span)
         (srcloc (app clean-source src) line column position span))
     (list src line column position span)]
    [(? syntax? stx)
     (list (clean-source (syntax-source stx))
           (syntax-line stx)
           (syntax-column stx)
           (syntax-position stx)
           (syntax-span stx))]
    [_ (raise-argument-error '->srcloc-as-list
                             "syntax? or srcloc as list? or vector?"
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

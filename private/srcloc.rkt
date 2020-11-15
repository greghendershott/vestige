#lang racket/base

(provide srcloc->list
         stx->srcloc-list)

(define (srcloc->list srcloc)
  (list (clean-source (srcloc-source srcloc))
        (srcloc-line srcloc)
        (srcloc-column srcloc)
        (srcloc-position srcloc)
        (srcloc-span srcloc)))

(define (stx->srcloc-list stx)
  (list (clean-source (syntax-source stx))
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

(define (clean-source src)
  (or (and src (path? src) (path->string src))
      src))

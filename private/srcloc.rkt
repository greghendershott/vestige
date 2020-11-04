#lang racket/base

(require racket/format)

(provide stx->srcloc)

(define (stx->srcloc stx)
  (define src (syntax-source stx))
  (list (or (and src (path? src) (path->string src))
            src)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx))
  #;
  (~a #:separator ":"
      (or (and src (path? src) (path->string src))
          src)
      (syntax-line stx)
      (syntax-column stx)
      (syntax-position stx)
      (syntax-span stx)))

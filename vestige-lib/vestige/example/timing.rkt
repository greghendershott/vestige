#lang racket/base

(module count racket/base
  (define count 1000)
  (provide count))

(module not-traced racket/base
  (require racket/match)
  (define (f x) (+ 1 (g x)))
  (define (g x) (let loop ([x 100])
                  (if (zero? x) (h x) (loop (sub1 x)))))
  (define (h x) (+ 1 x))
  (define (recur xs)
    (match xs
      [(list) (list)]
      [(cons x more) (cons (+ 100 x) (recur more))]))
  (require (submod ".." count))
  (time (for ([x (in-range count)])
          (+ 1 (f x))
          (recur (build-list 1000 values)))))

(module traced racket/base
  (require vestige/tracing/implicit)

  (require racket/match)
  (define (f x) (+ 1 (g x)))
  (define (g x) (let loop ([x 100])
                  (if (zero? x) (h x) (loop (sub1 x)))))
  (define (h x) (+ 1 x))
  (define (recur xs)
    (match xs
      [(list) (list)]
      [(cons x more) (cons (+ 100 x) (recur more))]))
  (require (submod ".." count))
  (time (for ([x (in-range count)])
          (+ 1 (f x))
          (recur (build-list 1000 values)))))

(require 'not-traced
         'traced)

#lang racket/base

(module intercepted-logger-example racket/base
  (require racket/logging
           racket/match
           vestige/explicit
           vestige/logger
           json)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (f 42)
    (trace-expression (* 2 3)))
  (define interceptor
    (match-lambda [(vector _level _str value _topic)
                   (displayln (jsexpr->string value))]))
  (with-intercepted-logging interceptor example #:logger logger level topic))

(module with-vestiges-example racket/base
  (require vestige/explicit
           vestige/logger)
  (with-vestiges
    (trace-define (f x) (+ 1 x))
    (f 42)
    (trace-expression (* 2 3))))

(module explicit-example racket/base
  (require vestige/explicit)
  (provide explicit-example)
  (define (explicit-example)
    (trace-define (baz x) x)
    (trace-define (foo x
                       #:kw kw
                       #:keyword [keyword 42])
      (baz x)
      (trace-let loop ([x 4])
        (if (zero? x) (baz x) (loop (sub1 x)))))
    (trace-define (bar x) (+ (foo x #:kw #f) 1))
    (trace-define (hello x) (+ (bar x)))
    (hello 42)
    (trace-define ((curried x0 x1) y0 y1)
      (+ x0 x1 y0 y1))
    (trace-define uncurried (curried 1 2))
    (uncurried 3 4)
    (trace-expression (void))
    (define x 42)
    (define y 1)
    (trace-expression (+ x (trace-expression (+ y 3))))
    (trace-expression (+ 1 2))
    (define alice (lambda (x) x))
    (alice 34))) ;another tail call

(module implicit-example racket/base
  (require vestige
           racket/match)
  (provide implicit-example)
  (define (implicit-example)
    (define (baz x) x)
    (define (foo x)
      (baz x)
      (let loop ([x 4])
        (if (zero? x) (baz x) (loop (sub1 x)))))
    (define (bar x) (+ (foo x) 1))
    (define (hello x) (+ (bar x)))
    (hello 42)
    (define ((curried x) y)
      (+ x y))
    (define uncurried (curried 99))
    (uncurried 1)
    (trace-expression (void))
    (trace-expression (+ 1 2))
    (define (recur xs)
      (match xs
        [(list) (list)]
        [(cons x more) (cons (+ 1 x) (recur more))]))
    (recur (list 1 2 3 4 5 6 7 8 9 10))
    (define alice (lambda (x) x))
    (alice 34))) ;another tail call

(require vestige/logger)

(require 'explicit-example
         'implicit-example)
(with-vestiges (explicit-example))
(with-vestiges (implicit-example))

(module thread-example racket/base
  (require vestige)
  (define (foo x) x)
  (define (thread-example)
    (define (t1-thunk) (for ([n 2]) (foo n) (sleep 0)))
    (define (t2-thunk) (for ([n 2]) (foo n) (sleep 0)))
    (define t1 (thread t1-thunk))
    (define t2 (thread t2-thunk))
    (for (#:when (and (thread-running? t1)
                      (thread-running? t2)))
      (sleep 1)))
  (provide thread-example))
(require 'thread-example)
(with-vestiges (thread-example))

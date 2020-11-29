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

(module explicit-example racket/base
  (require vestige/explicit)
  (provide explicit-example)
  (define (explicit-example)
    (trace-define (baz x) x)
    (trace-define (foo x #:kw kw)
      (baz x)
      (trace-let loop ([x 4])
        (if (zero? x) (baz x) (loop (sub1 x)))))
    (trace-define (bar x) (+ (foo x #:kw #f) 1))
    (trace-define (hello x) (+ (bar x)))
    (hello 42)
    (trace-expression (void))
    ;;(trace-expression (+ 1 2))
    ;;(trace-expression (values 1 2 3))
    (define x 42)
    (define y 1)
    (trace-expression (+ x (trace-expression (+ y 3))))
    ))

(module implicit-example racket/base
  (require vestige)
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
    (trace-expression (void))
    ;;(trace-expression (+ 1 2))
    ;;(trace-expression (values 1 2 3))
    ))

(require racket/logging
         racket/match
         racket/pretty
         vestige/logger)

(define (show-logged-example proc)
  (define interceptor (match-lambda [(vector _level str val _topic)
                                     (pretty-print (list str val))]))
  (with-intercepted-logging interceptor proc #:logger logger level topic))

(require 'explicit-example
         'implicit-example)

(show-logged-example explicit-example)
(show-logged-example implicit-example)

(module m racket/base
  (require vestige/explicit)
  (trace-define (f x)
    (trace-let loop ([x x])
      (if (zero? x)
          x
          (loop (sub1 x)))))
  (provide f))

(require 'm)
;;(require vestige/explicit) ;for #%app
;;(show-logged-example (λ () (f 2)))


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
(show-logged-example thread-example)

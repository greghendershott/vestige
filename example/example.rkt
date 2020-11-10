#lang racket/base

(module intercepted-logger-example racket/base
  (require racket/logging
           racket/format
           racket/match
           vestige/explicit
           vestige/logger
           json)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (f 42)
    (trace-expression (* 2 3)))
  (define (->jsexpr v)
    (cond [(hash? v)   (for/hasheq ([(k v) (in-hash v)])
                         (values k (->jsexpr v)))]
          [(list? v)   (map ->jsexpr v)]
          [(number? v) v]
          [else        (~a v)]))
  (define interceptor
    (match-lambda [(vector _level _str value _topic)
                   (displayln (jsexpr->string (->jsexpr value)))]))
  (with-intercepted-logging interceptor example #:logger logger level topic))

(module explicit-example racket/base
  (require vestige/explicit)
  (provide explicit-example)
  (define (explicit-example)
    (define (baz x) x)
    (trace baz) ;plain trace will show srcloc for trace site not definition site
    (trace-define (foo x #:kw kw)
      (baz x)
      (trace-let loop ([x 4]) (if (zero? x) (baz x) (loop (sub1 x)))))
    (trace-define (bar x) (+ (foo x #:kw #f) 1))
    (trace-define (hello x) (+ (bar x)))
    (hello 42)
    (trace-expression (void))
    (trace-expression (+ 1 2))
    (trace-expression (values 1 2 3))
    (trace-expression (+ 1 (trace-expression (+ 2 3))))))

(module implicit-example racket/base
  (require vestige)
  (provide implicit-example)
  (define (implicit-example)
    (define (baz x) x)
    (define (foo x #:kw kw)
      (baz x)
      (let loop ([x 4]) (if (zero? x) (baz x) (loop (sub1 x)))))
    (define (bar x) (+ (foo x #:kw #f) 1))
    (define (hello x) (+ (bar x)))
    (hello 42)
    (trace-expression (void))
    (trace-expression (+ 1 2))
    (trace-expression (values 1 2 3))))

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

(show-logged-example implicit-example)
(show-logged-example explicit-example)

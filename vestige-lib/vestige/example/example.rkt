#lang racket/base

;; These examples are mostly intended for my own use in developing
;; vestige as well as exercising its use by Racket Mode.

;; This submodule installs a log receiver to pretty-print the logger
;; events. Putting this in a submodule ensures it runs before the
;; other submodules run! (If this were at the top, file module level,
;; it would run later.)
(module start-receiver racket/base
  (require (submod "../receiving.rkt" private start)))
(require 'start-receiver)

;; This module is an example of using the explicit trace-x forms.
(module explicit-example racket/base
  (require vestige/tracing
           vestige/app
           vestige/logging)
  (define-logger example)
  (log-example-info "not in any with-more-logging-data form")
  (with-more-logging-data (log-example-info "outside any traced function"))
  (trace-define (baz x) x)
  (trace-define (foo x)
    (log-example-info "inside foo but not in any with-more-logging-data form")
    (with-more-logging-data (log-example-info "inside"))
    (with-more-logging-depth
      (with-more-logging-data (log-example-info "inside, nested")))
    (baz x)
    (trace-let loop ([x 4])
      (if (zero? x) (baz x) (loop (sub1 x)))))
  (trace-define (bar x) (+ (foo x) 1))
  (trace-define (hello x #:kw kw [y 2] [z (baz 12)])
    (+ (bar x) y kw z))
  (hello 42 #:kw 12)
  (trace-define (rest-arg . args) args)
  (rest-arg 0 1 2)
  (define alice (trace-lambda (x) x))
  (with-more-logging-data
    (log-example-info "Stats"))
  (alice 34))
(require 'explicit-example)

;; This module is an example of letting vestige forms shadow the
;; racket/base ones.
(module implicit-example racket/base
  (require vestige/tracing/implicit
           vestige/logging
           racket/match)
  (define (baz x) x)
  (define (foo x)
    (baz x)
    (let loop ([x 4])
      (if (zero? x) (baz x) (loop (sub1 x)))))
  (define (bar x) (+ (foo x) 1))
  (define (hello x) (+ (bar x)))
  (hello 42)
  (log-expression (void))
  (log-expression (+ 1 2))
  (let ([x 1] [y 2])
   (log-expression (+ x (log-expression (+ y 3)))))
  (log-expression (values 1 2))
  (define cl (case-lambda
               [() 0]
               [(x) x]
               [(x y) (+ x y)]))
  (cl)
  (cl 1)
  (cl 2 3)
  (define (recur xs)
    (match xs
      [(list) (list)]
      [(cons x more) (cons (+ 100 x) (recur more))]))
  (recur (list 1 2 3 4 5 6 7 8 9 10))
  (define ((curried x) y)
    (+ x y))
  (define uncurried (curried 99))
  (uncurried 1)
  (define alice (lambda (x) x))
  (alice 34))
(require 'implicit-example)

(module thread-example racket/base
  (require vestige/tracing/implicit)
  (define (foo x) x)
  (define (t1-thunk) (for ([n 2]) (foo n) (sleep 0)))
  (define (t2-thunk) (for ([n 2]) (foo n) (sleep 0)))
  (define t1 (thread t1-thunk))
  (define t2 (thread t2-thunk))
  (for (#:when (and (thread-running? t1)
                    (thread-running? t2)))
    (sleep 1)))
(require 'thread-example)

(module hash-update-example racket/base
  (require vestige/tracing/implicit
           racket/set)
  (define ht (make-hash))
  (define (add k v)
    (hash-update! ht
                  k
                  (λ (s) (set-add s v))
                  (set)))
  (add 'key 0)
  (add 'key 1)
  ht)
(require 'hash-update-example)

(module m racket/base
  (require vestige/tracing
           (submod vestige/receiving private start))
  ((trace-lambda #:name foo (x) x) 1))

(module class-example racket/base
  (require vestige/tracing/class)
  (define fish%
    (class object%
      (init size)                ; initialization argument
      (define current-size size) ; field
      (super-new)                ; superclass initialization
      (define/public (get-size)
        current-size)
      (trace-define/public (grow amt)
                           (set! current-size (+ amt current-size)))
      (define/public (eat other-fish)
        (grow (send other-fish get-size)))))

  (define picky-fish%
    (class fish% (super-new)
      (trace-define/override (grow amt)
                             (super grow (* 3/4 amt)))))

  (define daisy (new picky-fish% [size 20]))
  (send daisy get-size)
  (send daisy grow 100)
  (send daisy get-size)

  (define charlie (new fish% [size 500]))
  (send daisy eat charlie))
(require 'class-example)

;; This example module here just to compare check-syntax tail
;; reporting for known good examples to our own.
(module racket/trace racket/base
  (require racket/trace)
  (trace-define (baz x) x)
  (trace-define (foo x)
    (baz 12)
    (trace-let loop ([x 4])
      (if (zero? x) (baz x) (loop (sub1 x)))))
  (trace-define (hello x) (baz x))
  (define alice (lambda (x) x))
  (alice 34))

;; This example module here just to compare check-syntax tail
;; reporting for known good examples to our own.
(module normal racket/base
  (define (baz x) x)
  (define (foo x)
    (baz 12)
    (let loop ([x 4])
      (if (zero? x) (baz x) (loop (sub1 x)))))
  (define (hello x) (baz x))
  (define cl (case-lambda
               [() 0]
               [(x) x]
               [(x y) (+ x y)])))

;; Finally stop the receiver, letting it print the accumulated output,
;; after module evaluations have printed results, not intermixed.
(module stop-receiver racket/base
  (require (submod vestige/receiving private stop)))
(require 'stop-receiver)

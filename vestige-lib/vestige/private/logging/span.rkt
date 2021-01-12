#lang racket/base

(require racket/format
         "data.rkt"
         "depth.rkt"
         "log.rkt")

(define (call-with-logging-span thunk
                                #:wind?  [wind? #t]
                                #:name   [name #f]
                                #:start  [start default-start]
                                #:finish [finish default-finish])
  (define guid (new-guid))
  (define beg (current-inexact-milliseconds))
  (define (pre-thunk)
    (start guid name beg))
  (define (value-thunk)
    (with-more-logging-depth (thunk)))
  (define (post-thunk)
    (finish guid name beg (current-inexact-milliseconds)))
  (define (not-wind pre val post)
    (pre)
    (begin0 (val)
      (post)))
  (with-more-logging-data
    (with-more-logging-depth
      ((if wind? dynamic-wind not-wind)
       pre-thunk value-thunk post-thunk))))

(define (default-start guid name beg)
  (when (log?)
    (log! (~a #:separator " "
              "start span" guid (~v name) beg))))

(define (default-finish guid name beg end)
  (when (log?)
    (log! (~a #:separator " "
              "finish span" guid (~v name) beg end (- end beg)))))

(define guid (box 0))
(define (new-guid)
  (let try ()
    (define old (unbox guid))
    (define new (add1 old))
    (if (box-cas! guid old new)
        new
        (try))))

(call-with-logging-span #:name "foo"
                        #:wind? #f
                        (λ ()
                          (sleep 1)
                          (log! "hi")
                          (sleep 1)
                          (error 'error)
                          42))

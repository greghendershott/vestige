#lang racket/base

(require racket/contract
         racket/match
         "../in-marks.rkt"
         "../logging/app.rkt"
         "../logging/depth.rkt"
         "../logging/srcloc.rkt"
         "logging.rkt")

(provide make-wrapper-proc)

(define/contract (make-wrapper-proc proc
                                    name
                                    definition-srcloc
                                    header-srcloc
                                    formals-srcloc
                                    positional-syms)
  (-> procedure?
      symbol?
      srcloc-as-list/c
      srcloc-as-list/c
      srcloc-as-list/c
      (or/c (listof symbol?) symbol?)
      procedure?)
  (define called (make-called-hash-table definition-srcloc
                                         header-srcloc
                                         formals-srcloc))
  (define (call-traced kws kw-vals args)
    (define caller (cms->caller proc))
    (define old-depth (or (continuation-mark-set-first #f depth-key) 0))
    (define new-depth (add1 old-depth))
    ;; Tentatively push the new depth:
    (with-continuation-mark depth-key new-depth
      ;; Check first two new marks; consecutive?
      (match (for/list ([v (in-marks (current-continuation-marks) depth-key)]
                        [_ (in-range 2)])
               v)
        [(list this next)
         #:when (> this (add1 next)) ;e.g. (4 2) instead of (3 2)
         ;; Tail call: keep old depth and call `proc`. We don't print
         ;; the results, because the original call will.
         (with-continuation-mark depth-key old-depth
           (begin
             (log-args name #t args kws kw-vals caller called positional-syms)
             (if (null? kws)
                 (apply proc args)
                 (keyword-apply proc kws kw-vals args))))]
        [_
         ;; Not a tail call; push old depth, again, to ensure that
         ;; when we push the new depth, we have consecutive depths
         ;; associated with the mark (i.e., set up for tail-call
         ;; detection the next time around):
         (with-continuation-mark depth-key old-depth
           (call-with-values
            (λ ()
              (with-continuation-mark depth-key new-depth
                (begin
                  (log-args name #f args kws kw-vals caller called positional-syms)
                  (if (null? kws)
                      (apply proc args)
                      (keyword-apply proc kws kw-vals args)))))
            (λ results
              (with-continuation-mark depth-key new-depth
                (log-results name results caller called))
              (apply values results))))])))
  (define (plain-proc . args)          (call-traced null null    args))
  (define (kw-proc kws kw-vals . args) (call-traced kws  kw-vals args))
  (procedure-rename (make-keyword-procedure kw-proc plain-proc)
                    name))

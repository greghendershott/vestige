#lang racket/base

(require racket/contract
         "../logging/app.rkt"
         "../logging/depth.rkt"
         "../logging/log.rkt"
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
  (define (traced kws kw-vals args)
    (define caller (cms->caller proc))
    (define old-depth (or (continuation-mark-set-first #f depth-key) 0))
    (define new-depth (add1 old-depth))
    ;; Tentatively push the new depth:
    (with-continuation-mark depth-key new-depth
      (cond
        [(tail?)
         ;; Tail call: keep old depth and call `proc`. We don't print
         ;; the results, because the original call will.
         (with-continuation-mark depth-key old-depth
           (begin
             (log-args name #t args kws kw-vals caller called positional-syms)
             (if (null? kws)
                 (apply proc args)
                 (keyword-apply proc kws kw-vals args))))]
        [else
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
  (define (plain-proc . args)
    (if (log?)
        (traced null null args)
        (apply proc args)))
  (define (kw-proc kws kw-vals . args)
    (if (log?)
        (traced kws kw-vals args)
        (keyword-apply proc kws kw-vals args)))
  (procedure-rename (make-keyword-procedure kw-proc plain-proc)
                    name))

(define (tail?)
  ;; We need to check the first two marks, if any, for tail detection.
  ;;
  ;; We'd like to write:
  #;
  (let ()
    (local-require racket/match "../in-marks.rkt")
    (match (for/list ([v (in-marks (current-continuation-marks) depth-key)]
                      [_ (in-range 2)])
             v)
      [(list)                      #f]
      [(list depth)                #f]
      [(list depth previous-depth) (> depth (add1 previous-depth))]))
  ;; But that has allocations for the list. Slightly faster:
  #;
  (let ()
    (local-require racket/match "../in-marks.rkt")
    (match (for/vector #:length 2 #:fill #f
                       ([v (in-marks (current-continuation-marks) depth-key)])
                       v)
      [(vector #f    #f)             #f]
      [(vector depth #f)             #f]
      [(vector depth previous-depth) (> depth (add1 previous-depth))]))
  ;; But that still has some allocation for the vector and some
  ;; overhead from in-marks. The following avoids as much allocation
  ;; as we can. It provides a modest but measurable improvement in
  ;; micro benchmarks.
  (let*-values ([(iter) (continuation-mark-set->iterator
                         (current-continuation-marks)
                         (list depth-key))]
                [(v0 iter) (iter)])
    (if v0
        (let-values ([(old-depth) (vector-ref v0 0)]
                     [(v1 _) (iter)])
          (if v1
              (let* ([next-oldest (vector-ref v1 0)]
                     [tail? (> old-depth (add1 next-oldest))]) ;e.g. (4 2) not (3 2)
                tail?)
              #f))
        #f)))

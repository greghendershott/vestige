#lang racket/base

(require racket/contract
         "../marks.rkt"
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
    (define caller (cms->caller wrapped-proc))
    (define old-depth (or (continuation-mark-set-first #f depth-key) 0))
    (define new-depth (add1 old-depth))
    ;; Let's say the list of depth marks now is e.g. (1 0). We know
    ;; the old depth was 1. But we don't know whether we were called
    ;; in a tail position. To learn that, we need to add a mark and
    ;; see what happens:
    (with-continuation-mark depth-key new-depth
      ;; If we were NOT called in tail position, there will have been
      ;; a new continuation frame to which the mark was added. As a
      ;; result, the marks list will have changed from e.g. (1 0) to
      ;; (2 1 0). In the new list the first two elements are
      ;; consecutive: (2 1).
      ;;
      ;; If we WERE called in tail position, there was no new
      ;; continuation frame; that's what tail position means: same
      ;; continuation. In that case the mark replaced any existing
      ;; mark in the original frame. As a result, the marks list will
      ;; have changed from e.g. (1 0) to (2 0). In the new list, the
      ;; first two marks are not consecutive: (2 0).
      ;;
      ;; `tail?` checks whether the first two marks are consecutive.
      (cond
        [(tail?)
         ;; Keep old depth. Call `proc`. Don't print results.
         (with-continuation-mark depth-key old-depth ;e.g. (1 0)
           (begin
             (log-args name #t args kws kw-vals caller called positional-syms)
             (if (null? kws)
                 (apply proc args)
                 (keyword-apply proc kws kw-vals args))))]
        [else
         ;; Push old depth, again, to ensure that when we push the new
         ;; depth, we have consecutive depths (i.e. we're set up for
         ;; tail detection the next time around):
         (with-continuation-mark depth-key old-depth ;e.g. (1 1 0)
           (call-with-values
            (λ ()
              (with-continuation-mark depth-key new-depth ;e.g. (2 1 1 0)
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
  (define wrapped-proc
    (procedure-rename (make-keyword-procedure kw-proc plain-proc)
                      name))
  wrapped-proc)

(define (tail?)
  ;; We need to check the first two marks, if any, for tail detection.
  ;; Conceptually:
  ;;
  ;; (require racket/match "../marks.rkt")
  ;; (match (for/list ([v (in-marks #f depth-key)]
  ;;                   [_ (in-range 2)])
  ;;          v)
  ;;   [(list depth previous-depth) (> depth (add1 previous-depth))]
  ;;   [_                           #f])
  ;;
  ;; Although for/list allocations can be mitigated by using
  ;; for/vector with #:length, the fact remains that in-marks adds
  ;; some overhead creating a sequence, producing it, and checking for
  ;; its end.
  ;;
  ;; The following "bespoke" code avoids as much allocation as we can.
  ;; Furthermore it uses our `cms->iterator` that supplies #f to
  ;; continuation-mark-set->iterator when supported (this can be
  ;; somewhat faster, as described in the docs for
  ;; continuation-mark-set-first, which always/officially accepts #f).
  ;;
  ;; These micro optimizations provide a modest but measurable
  ;; improvement in micro benchmarks, such as when called tens of
  ;; thousands of times (which is a not unlikely scenario for us).
  (let*-values ([(iter0)    (cms->iterator #f (list depth-key))]
                [(v0 iter1) (iter0)])
    (and v0
         (let-values ([(this-depth) (vector-ref v0 0)]
                      [(v1 _iter2)  (iter1)])
           (and v1
               (let ([prev-depth (vector-ref v1 0)])
                 (> this-depth (add1 prev-depth)))))))) ;e.g. (2 0) not (1 0)

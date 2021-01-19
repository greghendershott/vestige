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
  ;; Conceptually:
  ;;
  ;; (require racket/match "../in-marks.rkt")
  ;; (match (for/list ([v (in-marks (current-continuation-marks) depth-key)]
  ;;                   [_ (in-range 2)])
  ;;          v)
  ;;   [(list depth previous-depth) (> depth (add1 previous-depth))]
  ;;   [_                           #f])
  ;;
  ;; But that has allocations for the list, and, in-marks has some
  ;; overhead creating the sequence producer.
  ;;
  ;; The following "bespoke" code avoids as much allocation as we can.
  ;; It provides a modest but measurable improvement in micro
  ;; benchmarks when called tens of thousands of times (which is a not
  ;; unlikely scenario for us).
  ;;
  ;; Also: At least in 7.8 CS, `continuation-mark-set->iterator` takes
  ;; the same `#f` shorthand for `(current-continuation-marks)` as
  ;; does continuation-mark-set-first --- and, more importantly, it
  ;; enables a similar shortcut/speedup. As I type this, not yet sure
  ;; whether that's official and OK to rely on.
  ;;
  ;; Update: Racket CS supports #f by accident; Racket BC doesn't
  ;; support this. Possibly Matthew will generalize BC and the docs to
  ;; support it officially. I'll leave this for now, pending learning
  ;; more. [This library is still "unstable" so it's possible I could
  ;; require say Racket 8.0 to support using #f, or, (sad-face) revert
  ;; back to supplying (c-c-m). Will see.]
  (let*-values ([(iter0)    (continuation-mark-set->iterator #f (list depth-key))]
                [(v0 iter1) (iter0)])
    (and v0
         (let-values ([(this-depth) (vector-ref v0 0)]
                      [(v1 _iter2)  (iter1)])
           (and v1
               (let ([prev-depth (vector-ref v1 0)])
                 (> this-depth (add1 prev-depth)))))))) ;e.g. (4 2) not (3 2)

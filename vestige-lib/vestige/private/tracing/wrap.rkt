#lang racket/base

(require racket/contract
         racket/match
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
  (define (apply/traced kws kw-vals args)
    (define caller (cms->caller proc))
    (define depths (continuation-mark-set->list
                    (current-continuation-marks)
                    depth-key))
    (define depth (if (null? depths) 0 (car depths)))
    ;; Tentatively push the new depth depth:
    (with-continuation-mark depth-key (add1 depth)
      ;; Check for tail-call => car of depths replaced,
      ;;  which means that the first two new marks are
      ;;  not consecutive:
      (let ([new-depths (continuation-mark-set->list
                         (current-continuation-marks)
                         depth-key)])
        (cond
          [(and (pair? (cdr new-depths))
                (> (car new-depths) (add1 (cadr new-depths))))
           ;; Tail call: reset depth and just call real-value. (This is
           ;; in tail position to the call to `apply/traced'.) We don't
           ;; print the results, because the original call will.
           (log-args name #t args kws kw-vals caller called positional-syms)
           (with-continuation-mark depth-key (car depths)
             (if (null? kws)
                 (apply proc args)
                 (keyword-apply proc kws kw-vals args)))]
          [else
           ;; Not a tail call; push the old depth, again, to ensure that
           ;; when we push the new depth, we have consecutive depths
           ;; associated with the mark (i.e., set up for tail-call
           ;; detection the next time around):
           (log-args name #f args kws kw-vals caller called positional-syms)
           (with-continuation-mark depth-key depth
             (call-with-values
              (λ ()
                (with-continuation-mark depth-key (add1 depth)
                  (if (null? kws)
                      (apply proc args)
                      (keyword-apply proc kws kw-vals args))))
              (λ results
                (with-continuation-mark depth-key (add1 depth)
                  (log-results name results caller called))
                (apply values results))))]))))
  (define (plain-proc . args)          (apply/traced null null    args))
  (define (kw-proc kws kw-vals . args) (apply/traced kws  kw-vals args))
  (make-keyword-procedure kw-proc plain-proc))

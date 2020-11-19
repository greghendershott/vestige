#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         racket/match
         "logger.rkt")

(provide apply-traced
         tracing-#%app)

;; Key used for a continuation mark to indicate the nesting depth.
(define level-key (make-continuation-mark-key 'level))

;; NOTE: This is substantially copied from racket/trace, but enhanced
;; to grab caller and context if any from marks.
;;
;; Apply a traced procedure to arguments, printing arguments and
;; results. We set and inspect the level-key continuation mark a few
;; times to detect tail calls.
(define (apply-traced id-stx untraced-proc args kws kw-vals)
  (define levels (continuation-mark-set->list (current-continuation-marks)
                                              level-key))
  (define level (if (null? levels) 0 (car levels)))
  ;; Tentatively push the new depth level:
  (with-continuation-mark level-key (add1 level)
    ;; Check for tail-call => car of levels replaced, which means
    ;; that the first two new marks are not consecutive:
    (let* ([marks (current-continuation-marks)]
           [new-levels (continuation-mark-set->list marks level-key)]
           [caller (caller-srcloc marks)]
           [context (context-srcloc marks)])
      (cond
        [(and (pair? (cdr new-levels))
              (> (car new-levels) (add1 (cadr new-levels))))
         ;; Tail call: reset level and just call untraced proc. (This
         ;; is in tail position to the call to `apply-traced'.) We
         ;; don't print the results, because the original call will.
         (log-args id-stx args kws kw-vals (sub1 level) caller context)
         (with-continuation-mark level-key (car levels)
           (if (null? kws)
               (apply untraced-proc args)
               (keyword-apply untraced-proc kws kw-vals args)))]
        [else
         ;; Not a tail call; push the old level, again, to ensure
         ;; that when we push the new level, we have consecutive
         ;; levels associated with the mark (i.e., set up for
         ;; tail-call detection the next time around):
         (log-args id-stx args kws kw-vals level caller context)
         (with-continuation-mark level-key level
           (call-with-values
            (λ ()
              (with-continuation-mark level-key (add1 level)
                (if (null? kws)
                    (apply untraced-proc args)
                    (keyword-apply untraced-proc kws kw-vals args))))
            (λ results
              (flush-output)
              (log-results id-stx results level caller context)
              (apply values results))))]))))

;;; srcloc of caller

;; Assuming called from a module using our tracing-#%app.

(define caller-key (make-continuation-mark-key 'caller))

(define-syntax (tracing-#%app stx)
  (syntax-parse stx
    [(_ x:expr more ...)
     #:with loc #`(vector #,@(->srcloc-as-list stx))
     (syntax/loc stx
       (with-continuation-mark caller-key loc
         (#%app x more ...)))]))

(define (caller-srcloc marks)
  (continuation-mark-set-first marks caller-key))

;;; srcloc of context

;; This will never be as precise as tracing-#%app. Instead, it will be
;; a span somewhere within which is the call site. Although this is
;; not precise enough for e.g. a tool that wants to show the call "in
;; situ" similar to a step debugger, it is useful information. For
;; example it's more than adequate for devops logging purposes, and
;; anyway in such cases our tracing-#%app might be avoided for
;; peformance.
(define (context-srcloc marks)
  (for/or ([id+srcloc (in-list (continuation-mark-set->context marks))])
    (match id+srcloc
      [(cons _id (and sl (struct* srcloc ([source src]))))
       #:when (and (not (equal? src (syntax-source #'this-file)))
                   (path-string? src)
                   (complete-path? src))
       sl]
      [_ #f])))

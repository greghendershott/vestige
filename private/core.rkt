#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         racket/match
         "logger.rkt")

(provide wrap-with-tracing
         tracing-#%app)

;; Given a procedure and identifier syntax, wrap the procedure in one
;; which calls apply-traced with plain or keyword arguments. The plain
;; vs. keyword complexity is so we can show keywords next to keyword
;; argument values, e.g. "(foo #:kw 3)" instead of "(foo 3)".
(define (wrap-with-tracing proc id-stx)
  (define arity (procedure-arity proc))
  (define-values (required allowed) (procedure-keywords proc))
  (define kw-proc (λ (kws vals . args)
                    (apply-traced id-stx proc args kws vals)))
  (define plain-proc (λ args
                       (apply-traced id-stx proc args null null)))
  (define traced-proc (make-keyword-procedure kw-proc plain-proc))
  (define reduced-proc (procedure-reduce-keyword-arity traced-proc arity required allowed))
  (define renamed-proc (procedure-rename reduced-proc (syntax-e id-stx)))
  renamed-proc)

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
           [caller (caller-srcloc marks)]
           [context (context-srcloc marks)])
      (match (continuation-mark-set->list marks level-key)
        [(list* this next _)
         #:when (> this (add1 next))
         ;; Tail call: reset level and just call untraced proc. (This
         ;; is in tail position to the call to `apply-traced'.) We
         ;; don't print the results, because the original call will.
         (log-args id-stx #t args kws kw-vals level caller context)
         (with-continuation-mark level-key (car levels)
           (if (null? kws)
               (apply untraced-proc args)
               (keyword-apply untraced-proc kws kw-vals args)))]
        [_
         ;; Not a tail call; push the old level, again, to ensure
         ;; that when we push the new level, we have consecutive
         ;; levels associated with the mark (i.e., set up for
         ;; tail-call detection the next time around):
         (log-args id-stx #f args kws kw-vals level caller context)
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
     (quasisyntax/loc stx
       (with-continuation-mark caller-key (vector #,@(->srcloc-as-list stx))
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
       #:when (and (path-string? src)
                   (complete-path? src)
                   (not (equal? (filename-directory src)
                                (filename-directory (syntax-source #'this-file)))))
       sl]
      [_ #f])))

(define (filename-directory p)
  (car (call-with-values (λ () (split-path p)) list)))

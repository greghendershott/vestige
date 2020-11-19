#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         racket/match
         "logger.rkt")

(provide trace-one-procedure
         tracing-#%app)

;; A traced-procedure struct instance acts like a procedure, but preserves
;; the original, too.
(struct traced-procedure (traced untraced)
  #:property prop:procedure (struct-field-index traced))

;; Install traced version of a given procedure. The traced version is
;; also given, so that it can be constructed to have a nice name.
(define (install-traced-procedure id proc setter traced-proc)
  (unless (procedure? proc)
    (error 'trace
           "the value of ~s is not a procedure: ~e" id proc))
  (unless (traced-procedure? proc)
    (setter (traced-procedure
             (let-values ([(a) (procedure-arity proc)]
                          [(req allowed) (procedure-keywords proc)])
               (procedure-reduce-keyword-arity traced-proc
                                               a
                                               req
                                               allowed))
             proc))))

;; Key used for a continuation mark to indicate the nesting depth.
(define level-key (make-continuation-mark-key 'level))

;; Apply a traced procedure to arguments, printing arguments and
;; results. We set and inspect the level-key continuation mark a few
;; times to detect tail calls.
(define (apply-traced id args kws kw-vals real-value)
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
         ;; Tail call: reset level and just call real-value. (This
         ;; is in tail position to the call to `apply-traced'.) We
         ;; don't print the results, because the original call will.
         (log-args id args kws kw-vals (sub1 level) caller context)
         (with-continuation-mark level-key (car levels)
           (if (null? kws)
               (apply real-value args)
               (keyword-apply real-value kws kw-vals args)))]
        [else
         ;; Not a tail call; push the old level, again, to ensure
         ;; that when we push the new level, we have consecutive
         ;; levels associated with the mark (i.e., set up for
         ;; tail-call detection the next time around):
         (log-args id args kws kw-vals level caller context)
         (with-continuation-mark level-key level
           (call-with-values
            (λ ()
              (with-continuation-mark level-key (add1 level)
                (if (null? kws)
                    (apply real-value args)
                    (keyword-apply real-value kws kw-vals args))))
            (λ results
              (flush-output)
              (log-results id results level caller context)
              (apply values results))))]))))

(define-syntax (trace-one-procedure stx)
  (syntax-parse stx
    [(_ id:id)
     (with-syntax ([tid (let ([tid (format "traced-~a" (syntax-e #'id))])
                          (datum->syntax #'id (string->symbol tid) #f))]
                   [kw-proc
                    (quasisyntax/loc #'id
                      (λ (kws vals . args)
                        (apply-traced #'id args kws vals real-value)))]
                   [plain-proc
                    (quasisyntax/loc #'id
                      (λ args
                        (apply-traced #'id args null null real-value)))])
       #'(install-traced-procedure
          'id
          id
          (λ (v) (set! id v))
          (let* ([real-value id]
                 [tid (make-keyword-procedure kw-proc plain-proc)])
            tid)))]))

;;; srcloc of caller -- assuming called from a module using our
;;; tracing-#%app.

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
;;;
;; This will never be as precise as the calling expression. Instead, a
;; span that is the body of some function, somewhere within which is
;; the call site. Although this is not precise enough for e.g. a tool
;; that wants to show the call "in situ" similar to a step debugger,
;; it is a useful fallback. Furthermore it's more than adequate for
;; devops logging purposes, and anyway in those cases our
;; tracing-#%app might be avoided for peformance.
(define (context-srcloc marks)
  (for/or ([id+srcloc (in-list (continuation-mark-set->context marks))])
    (match id+srcloc
      [(cons _id (and sl (struct* srcloc ([source src]))))
       #:when (and (not (equal? src (syntax-source #'this-file)))
                   (path-string? src)
                   (complete-path? src))
       sl]
      [_ #f])))

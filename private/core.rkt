#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         racket/match
         "logger.rkt")

(provide make-chaperone-wrapper-proc
         tracing-#%app
         (rename-out [impersonator-prop:application-mark chaperone-prop-key])
         chaperone-prop-val)

;; Key used for a continuation mark to indicate the nesting depth.
(define level-key (make-continuation-mark-key 'level))

;; A value for impersonator-prop:application-mark.
(define chaperone-prop-val (cons level-key 'app-mark))

(define (make-chaperone-wrapper-proc id-stx)
  ;; Produces a wrapper proc for chaperone-procedure. The level
  ;; detection -- including tail call detection! -- relies on
  ;; chaperone-procedure also being called with chaperone prop/val
  ;; impersonator-prop:application-mark (cons level-key 'app-mark).
  ;; Then we can detect a tail call from the marks starting with two
  ;; such 'app-marks. When not a tail call, we add our own level-key
  ;; mark whose value is the level number.
  ;;
  ;; Honest note to future self: The "double app-mark" pattern arose
  ;; inductively/empirically -- not (yet) from a rigorous
  ;; understanding of how the 'app-mark values and the number values
  ;; interleave.
  ;;
  ;; Quote from docs, where I've capitalized PROC (the raw proc
  ;; being wrapped) and WRAPPER-PROC (this here).
  ;;
  ;; "If any prop is impersonator-prop:application-mark and if the
  ;; associated prop-val is a pair, then the call to PROC is wrapped
  ;; with with-continuation-mark using (car prop-val) as the mark key
  ;; and (cdr prop-val) as the mark value. In addition, if the
  ;; immediate continuation frame of the call to the impersonated
  ;; procedure includes a value for (car prop-val)—that is, if
  ;; call-with-immediate-continuation-mark would produce a value for
  ;; (car prop-val) in the call’s continuation—then the value is also
  ;; installed as an immediate value for (car prop-val) as a mark
  ;; during the call to WRAPPER-PROC (which allows tail-calls of
  ;; impersonators with respect to wrapping impersonators to be
  ;; detected within wrapper-proc)."
  ;;
  ;; <https://docs.racket-lang.org/reference/chaperones.html?q=impersonate-procedure#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._impersonate-procedure%29%29>
  (define (on-args kws kw-vals args)
    (let* ([cms (current-continuation-marks)]
           ;; OPTIMIZE: Use cms->iterator to grab marks until we reach a number
           [marks (continuation-mark-set->list cms level-key)]
           [caller (caller-srcloc cms)]
           [context (context-srcloc cms)])
      (println marks)
      (match marks
        [(or
          ;; This first pattern is, IIUC, to work around <https://github.com/racket/racket/issues/1836>.
          (list* 'app-mark 'app-mark (? number?) (? number? level) _)
          (list* 'app-mark 'app-mark (? number? level) _))
         ;; Tail call: Do NOT supply a `results` proc to the
         ;; chaperone, because we don't want to log the results (the
         ;; original call will) and because we want this to remain a
         ;; tail call. Also, do NOT call the raw proc with a new,
         ;; incremented level-key mark.
         (log-args id-stx #t args kws kw-vals level caller context)
         (if (null? kws)
             (apply values         args)
             (apply values kw-vals args))]
        [_
         (define level (or (findf number? marks) 0))
         ;; Not a tail call: Do supply a `results` proc to the
         ;; chaperone so we can log the results. Also, ask it to call
         ;; the raw proc with an incremented level-key mark.
         (log-args id-stx #f args kws kw-vals level caller context)
         (define (on-results . results)
           (log-results id-stx results level caller context)
           (apply values results))
         (if (null? kws)
             (apply values on-results 'mark level-key (add1 level)         args)
             (apply values on-results 'mark level-key (add1 level) kw-vals args))])))
  (define (plain-proc . args)          (on-args null null    args))
  (define (kw-proc kws kw-vals . args) (on-args kws  kw-vals args))
  (make-keyword-procedure kw-proc plain-proc))

(module+ example
  (define f
    (chaperone-procedure
     (lambda (x)
       (if (zero? x)
           42
           (f (sub1 x))))
     (make-chaperone-wrapper-proc #'f)
     impersonator-prop:application-mark chaperone-prop-val))
  (f 3))

;;; srcloc of caller

;; Assuming called from a module using our tracing-#%app.

(define caller-key (make-continuation-mark-key 'caller))

(define-syntax (tracing-#%app stx)
  (syntax-parse stx
    [(_ x:expr more ...)
     (quasisyntax/loc stx
       (with-continuation-mark caller-key '#(#,@(->srcloc-as-list stx))
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

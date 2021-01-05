#lang racket/base

(require racket/match
         "../logging/depth.rkt"
         "id-stx-prop.rkt"
         "logging.rkt")

(provide make-chaperone-wrapper-proc
         (rename-out [impersonator-prop:application-mark chaperone-prop-key])
         chaperone-prop-val)

;; A value for impersonator-prop:application-mark.
(define chaperone-prop-val (cons depth-key 'app-mark))

(define (make-chaperone-wrapper-proc id-stx)
  ;; Produces a wrapper proc for chaperone-procedure. The depth
  ;; detection -- including tail call detection! -- relies on
  ;; chaperone-procedure also being called with chaperone prop/val
  ;; impersonator-prop:application-mark (cons depth-key 'app-mark).
  ;; Then we can detect a tail call from the marks starting with two
  ;; such 'app-marks. When not a tail call, we add our own depth-key
  ;; mark whose value is the depth number.
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
  (match-define (vector formals-srcloc header-srcloc positional-syms) (get-prop id-stx))
  (define (on-args kws kw-vals args)
    (let* ([cms (current-continuation-marks)]
           ;; OPTIMIZE: Use cms->iterator to grab marks until we reach a number
           [marks (continuation-mark-set->list cms depth-key)])
      ;;(println marks)
      (match marks
        [(or
          ;; This first pattern is, IIUC, to work around
          ;; <https://github.com/racket/racket/issues/1836>.
          (list* 'app-mark 'app-mark (? number?) (? number? depth) _)
          (list* 'app-mark 'app-mark (? number? depth) _))
         ;; Tail call: Do NOT supply a `results` proc to the
         ;; chaperone, because we don't want to log the results (the
         ;; original call will) and because we want this to remain a
         ;; tail call. Also, do NOT call the raw proc with a new,
         ;; incremented depth-key mark.
         (log-args id-stx #t args kws kw-vals depth
                   formals-srcloc header-srcloc positional-syms)
         (if (null? kws)
             (apply values         args)
             (apply values kw-vals args))]
        [_
         (define old-depth (marks->logging-depth marks))
         (define new-depth (add1 old-depth))
         ;; Not a tail call: Do supply a `results` proc to the
         ;; chaperone so we can log the results. Also, ask it to call
         ;; the raw proc with an incremented depth-key mark.
         (define (on-results . results)
           (with-continuation-mark depth-key new-depth
             (log-results id-stx results new-depth
                          formals-srcloc header-srcloc positional-syms))
           (apply values results))
         (with-continuation-mark depth-key new-depth
           (log-args id-stx #f args kws kw-vals new-depth
                     formals-srcloc header-srcloc positional-syms))
         (if (null? kws)
             (apply values on-results 'mark depth-key new-depth         args)
             (apply values on-results 'mark depth-key new-depth kw-vals args))])))
  (define (plain-proc . args)          (on-args null null    args))
  (define (kw-proc kws kw-vals . args) (on-args kws  kw-vals args))
  (make-keyword-procedure kw-proc plain-proc))

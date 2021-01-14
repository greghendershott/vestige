#lang racket/base

(require racket/contract
         racket/match
         "../in-marks.rkt"
         "../logging/app.rkt"
         "../logging/depth.rkt"
         "../logging/srcloc.rkt"
         "logging.rkt")

(provide make-chaperone-wrapper-proc
         (rename-out [impersonator-prop:application-mark chaperone-prop-key])
         chaperone-prop-val)

;; A value for impersonator-prop:application-mark.
(define chaperone-prop-val (cons depth-key 'app-mark))

;; Produce a wrapper proc for chaperone-procedure.
;;
;; The depth detection -- including tail call detection -- relies on
;; chaperone-procedure also being called with chaperone prop/val
;; impersonator-prop:application-mark (cons depth-key 'app-mark). That
;; way we can detect a tail call from the marks starting with two
;; consecutive 'app-mark values. When not a tail call, we add our own
;; depth-key mark whose value is the depth number.
;;
;; Honest note to future self: The "double app-mark" pattern arose
;; inductively/empirically -- not (yet) from a rigorous understanding
;; of how the 'app-mark values and the number values interleave.
;;
;; Quote from docs, where I've capitalized PROC (the proc being
;; wrapped) and WRAPPER-PROC (this here).
;;
;;   "If any prop is impersonator-prop:application-mark and if the
;;   associated prop-val is a pair, then the call to PROC is wrapped
;;   with with-continuation-mark using (car prop-val) as the mark key
;;   and (cdr prop-val) as the mark value. In addition, if the
;;   immediate continuation frame of the call to the impersonated
;;   PROCEDURE includes a value for (car prop-val) -- that is, if
;;   call-with-immediate-continuation-mark would produce a value for
;;   (car prop-val) in the call’s continuation -- then the value is
;;   also installed as an immediate value for (car prop-val) as a mark
;;   during the call to WRAPPER-PROC (which allows tail-calls of
;;   impersonators with respect to wrapping impersonators to be
;;   detected within WRAPPER-PROC)."
;;
;; <https://docs.racket-lang.org/reference/chaperones.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._impersonate-procedure%29%29>
(define/contract (make-chaperone-wrapper-proc proc
                                              name
                                              defn-srcloc
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
  (define called (make-called-hash-table defn-srcloc header-srcloc formals-srcloc))
  (define (on-args kws kw-vals args)
    (define caller (cms->caller proc))
    ;; For efficiency, don't get full list of marks. We only care
    ;; about those through the first one that is a number (if any).
    (match (for/list ([v (in-marks (current-continuation-marks) depth-key)]
                      #:final (number? v))
             v)
      [(or
        ;; This first pattern is, IIUC, to work around
        ;; <https://github.com/racket/racket/issues/1836>.
        (list* 'app-mark 'app-mark (? number?) (? number? depth) _)
        (list* 'app-mark 'app-mark (? number? depth) _))
       ;; Tail call: Do NOT supply a `results` proc to the chaperone,
       ;; because we don't want to log the results and because we want
       ;; to remain a tail call. Also, do NOT call the wrapped proc
       ;; with a new, incremented depth-key mark.
       (log-args name #t args kws kw-vals depth caller called positional-syms)
       (if (null? kws)
           (apply values         args)
           (apply values kw-vals args))]
      [marks
       ;; Not a tail call: Do supply a `results` proc to the
       ;; chaperone so we can log the results. Also, ask it to call
       ;; the wrapped proc with an incremented depth-key mark.
       (define old-depth (marks->logging-depth marks))
       (define new-depth (add1 old-depth))
       (define (on-results . results)
         (with-continuation-mark depth-key old-depth
           (log-results name results new-depth caller called))
         (apply values results))
       (log-args name #f args kws kw-vals new-depth caller called positional-syms)
       (if (null? kws)
           (apply values on-results 'mark depth-key new-depth         args)
           (apply values on-results 'mark depth-key new-depth kw-vals args))]))
  (define (plain-proc . args)          (on-args null null    args))
  (define (kw-proc kws kw-vals . args) (on-args kws  kw-vals args))
  (make-keyword-procedure kw-proc plain-proc))

#lang racket/base

(require racket/match
         "srcloc.rkt")

(provide add-signature-stx-prop
         get-signature-stx-prop)

(define property-key 'vestige-signature)

(define (add-signature-stx-prop stx . sig-stxs)
  (syntax-property stx
                   property-key
                   (merged-srcloc sig-stxs)
                   #t)) ;preserved

(define (get-signature-stx-prop stx)
  (syntax-property stx property-key))

;; Given one or more stxs, produce srcloc for the span from the first
;; to the last.
(define (merged-srcloc stxs)
  (match-define (cons first _) stxs)
  (define src  (syntax-source first))
  (define line (syntax-line first))
  (define col  (syntax-column first))
  (define pos  (syntax-position first))
  (match-define (cons last _) (reverse stxs))
  (define end (+ (syntax-position last) (syntax-span last)))
  (define span (- end pos))
  (->srcloc-as-list (vector src line col pos span)))

(module+ test
  (require rackunit)
  (define here #'here)
  (define there #'there)
  (let ([here here]
        [there there])
    (match-define (list src line col pos span) (merged-srcloc (list here there)))
    (check-equal? src (path->string (syntax-source here)))
    (check-equal? line (syntax-line here))
    (check-equal? col (syntax-column here))
    (check-equal? pos (syntax-position here))
    (check-equal? span (- (+ (syntax-position there) (syntax-span there))
                          (syntax-position here))))
  (let ([here here]
        [there here])
    (match-define (list src line col pos span) (merged-srcloc (list here there)))
    (check-equal? src (path->string (syntax-source here)))
    (check-equal? line (syntax-line here))
    (check-equal? col (syntax-column here))
    (check-equal? pos (syntax-position here))
    (check-equal? span (- (+ (syntax-position there) (syntax-span there))
                          (syntax-position here)))))

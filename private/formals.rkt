#lang racket/base

(require racket/match
         "srcloc.rkt")

(provide add-formals-stx-prop
         add-formals-stx-prop/whole-form
         get-formals-stx-prop)

(define property-key 'vestige-formals)

(define (add-formals-stx-prop stx formals-stxs)
  (syntax-property stx
                   property-key
                   (formals-srcloc formals-stxs)
                   #t)) ;preserved

(define (add-formals-stx-prop/whole-form to-stx from-stx)
  (syntax-property to-stx
                   property-key
                   (->srcloc-as-list from-stx)
                   #t)) ;preserved

(define (get-formals-stx-prop stx)
  (syntax-property stx property-key))

(define (formals-srcloc formals)
  (match (syntax->list formals)
    [(list single)           (merged-srcloc single single)]
    [(list first _ ... last) (merged-srcloc first last)]
    [(list)
     ;; Location is at the close paren
     (->srcloc-as-list
      (vector (syntax-source formals)
              (syntax-line formals)
              (+ (syntax-column formals) (sub1 (syntax-span formals)))
              (+ (syntax-position formals) (sub1 (syntax-span formals)))
              0))]))

;; Given two stxs, produce srcloc for the span from the first to the
;; last.
(define (merged-srcloc first last)
  (define src  (syntax-source first))
  (define line (syntax-line first))
  (define col  (syntax-column first))
  (define pos  (syntax-position first))
  (define end (+ (syntax-position last) (syntax-span last)))
  (define span (- end pos))
  (->srcloc-as-list (vector src line col pos span)))

(module+ test
  (require rackunit)
  (define here #'here)
  (define there #'there)
  (let ([here here]
        [there there])
    (match-define (list src line col pos span) (merged-srcloc here there))
    (check-equal? src (path->string (syntax-source here)))
    (check-equal? line (syntax-line here))
    (check-equal? col (syntax-column here))
    (check-equal? pos (syntax-position here))
    (check-equal? span (- (+ (syntax-position there) (syntax-span there))
                          (syntax-position here))))
  (let ([here here]
        [there here])
    (match-define (list src line col pos span) (merged-srcloc here there))
    (check-equal? src (path->string (syntax-source here)))
    (check-equal? line (syntax-line here))
    (check-equal? col (syntax-column here))
    (check-equal? pos (syntax-position here))
    (check-equal? span (- (+ (syntax-position there) (syntax-span there))
                          (syntax-position here)))))

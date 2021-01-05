#lang racket/base

(require (only-in racket/function negate)
         racket/match
         syntax/parse
         syntax/parse/lib/function-header
         "../logging/srcloc.rkt")

(provide add-prop
         get-prop)

(define prop-key 'vestige-syntax-property)

(define (add-prop stx
                  #:formals-stx formals-stx
                  #:header-stxs header-stxs)
  (syntax-property stx
                   prop-key
                   (vector (formals-srcloc formals-stx)
                           (header-srcloc header-stxs)
                           (formals->positionals formals-stx))
                   #t)) ;preserved

(define (get-prop stx)
  (syntax-property stx prop-key))

(define (header-srcloc stxs)
  (match stxs
    ;; Handle list with one or more syntaxes
    [(list one)              (->srcloc-as-list one)]
    [(list first _ ... last) (merge first last)]
    [_ (raise-syntax-error 'trace-lambda
                           "Expected a list of one or more syntaxes"
                           stxs)]))

(define (formals-srcloc formals)
  (match (syntax->list formals)
    ;; Handle formals being an identifier, e.g. (λ args _), in which
    ;; case the srcloc is simply that of the identifier syntax.
    [#f #:when (identifier? formals) (->srcloc-as-list formals)]
    ;; Handle formals being one or more parameters.
    [(list one)              (->srcloc-as-list one)]
    [(list first _ ... last) (merge first last)]
    ;; Handle formals being an empty list, e.g. (λ () _), i.e. no
    ;; parameters, by using location at the close paren.
    [(list)
     (->srcloc-as-list
      (list (syntax-source formals)
            (syntax-line formals)
            (+ (syntax-column formals) (sub1 (syntax-span formals)))
            (+ (syntax-position formals) (sub1 (syntax-span formals)))
            0))]
    ;; Should never get here if caller used the `formals` syntax class
    ;; from syntax/parse/lib/function-header, but just in case:
    [_ (raise-syntax-error 'trace-lambda
                           "Expected a list of syntax or an identifier"
                           formals)]))

(define (formals->positionals formals)
  (syntax-parse formals
    [id:id (syntax-e #'id)]
    [(fml:formal ...)
     (filter (negate keyword?)
             (syntax->datum #'((~? fml.kw fml.name) ...)))]))

;;; Utility

;; Given two stxs, produce srcloc for the span from the first to the
;; second.
(define (merge first last)
  (define src  (syntax-source first))
  (define line (syntax-line first))
  (define col  (syntax-column first))
  (define pos  (syntax-position first))
  (define end (+ (syntax-position last) (syntax-span last)))
  (define span (- end pos))
  (->srcloc-as-list (list src line col pos span)))

(module+ test
  (require rackunit)
  (define here #'here)
  (define there #'there)
  (let ([here here]
        [there there])
    (match-define (list src line col pos span) (merge here there))
    (check-equal? src (path->string (syntax-source here)))
    (check-equal? line (syntax-line here))
    (check-equal? col (syntax-column here))
    (check-equal? pos (syntax-position here))
    (check-equal? span (- (+ (syntax-position there) (syntax-span there))
                          (syntax-position here))))
  (let ([here here]
        [there here])
    (match-define (list src line col pos span) (merge here there))
    (check-equal? src (path->string (syntax-source here)))
    (check-equal? line (syntax-line here))
    (check-equal? col (syntax-column here))
    (check-equal? pos (syntax-position here))
    (check-equal? span (- (+ (syntax-position there) (syntax-span there))
                          (syntax-position here)))))

;; (define stx (add-stx-props #'base #:formals-stx #'(x y) #:header-stxs (list #'loop #'([x 0]))))
;; (get-formals-stx-prop stx)
;; (get-header-stx-prop stx)

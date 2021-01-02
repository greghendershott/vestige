#lang racket/base

(require racket/match
         "../logging/srcloc.rkt")

(provide add-loc-props
         add-loc-props/expression
         get-formals-stx-prop
         get-header-stx-prop)

;;; Adding the multiple special props

(define (add-loc-props stx #:formals-stx formals-stx #:header-stxs header-stxs)
  (add-formals-stx-prop (add-header-stx-prop stx (header-srcloc header-stxs))
                        (formals-srcloc formals-stx)))

(define (add-loc-props/expression stx expr-stx)
  (define loc (->srcloc-as-list expr-stx))
  (add-formals-stx-prop (add-header-stx-prop stx loc)
                        loc))

;;; The header prop

(define header-property-key  'vestige-header)

(define (add-header-stx-prop stx loc)
  (syntax-property stx header-property-key loc #t)) ;preserved

(define (header-srcloc stxs)
  (match stxs
    ;; Handle list with one or more syntaxes
    [(list one)              (->srcloc-as-list one)]
    [(list first _ ... last) (merge first last)]
    [_ (raise-syntax-error 'trace-lambda
                           "Expected a list of one or more syntaxes"
                           stxs)]))

(define (get-header-stx-prop stx)
  (syntax-property stx header-property-key))

;;; The formals prop

(define formals-property-key 'vestige-formals)

(define (add-formals-stx-prop stx loc)
  (syntax-property stx formals-property-key loc #t)) ;preserved

(define (get-formals-stx-prop stx)
  (syntax-property stx formals-property-key))

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
                           "Expected a list or an identifier"
                           formals)]))

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

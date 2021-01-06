#lang racket/base

(require (only-in racket/function negate)
         syntax/parse
         syntax/parse/lib/function-header
         "../logging/srcloc.rkt")

(provide formals-srcloc
         header-srcloc
         formals->positionals)

(module+ test
  (require rackunit
           racket/match))

(define (header-srcloc stxs)
  (syntax-parse stxs
    [(one)              (->srcloc-as-list #'one)]
    [(first _ ... last) (merge #'first #'last)]))

(module+ examples
  (header-srcloc #'(a))
  (header-srcloc #'(a b)))

(define (formals-srcloc formals)
  (syntax-parse formals
    [id:id              (->srcloc-as-list #'id)]
    [(one)              (->srcloc-as-list #'one)]
    [(first _ ... last) (merge #'first #'last)]
    [() ;handle thunks using loc of closing paren
     (->srcloc-as-list
      (list (syntax-source formals)
            (syntax-line formals)
            (+ (syntax-column   formals) (sub1 (syntax-span formals)))
            (+ (syntax-position formals) (sub1 (syntax-span formals)))
            0))]))

(module+ examples
  (formals-srcloc #'id)
  (formals-srcloc #'(a))
  (formals-srcloc #'(a b))
  (formals-srcloc #'(a b c))
  (formals-srcloc #'()))

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
  (require rackunit
           racket/match)
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

;; Return a list of symbols for the names of required or optional
;; POSITIONAL parameters -- but NOT required or optional KEYWORD
;; arguments.
(define (formals->positionals formals)
  (syntax-parse formals
    [id:id (syntax-e #'id)]
    [(fml:formal ...)
     (filter (negate keyword?)
             (syntax->datum #'((~? fml.kw fml.name) ...)))]))

(module+ test
  (check-equal? (formals->positionals #'())
                '())
  (check-equal? (formals->positionals #'(a b [c 0]))
                '(a b c))
  (check-equal? (formals->positionals #'(#:kw kw))
                '())
  (check-equal? (formals->positionals #'(#:kw [kw 0]))
                '())
  (check-equal? (formals->positionals #'(a #:kw kw [b 0] #:opt-kw [opt-kw 0]))
                '(a b)))

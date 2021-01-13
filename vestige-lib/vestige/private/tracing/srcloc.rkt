#lang racket/base

(require (only-in racket/function negate)
         syntax/parse
         syntax/parse/lib/function-header
         "../logging/srcloc.rkt")

(provide formals-srcloc
         header-srcloc
         formals->positionals
         ->srcloc-as-list)

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
(define (merge from thru)
  (unless (equal? (syntax-source from) (syntax-source thru))
    (raise-arguments-error 'merge "expected same syntax-source"
                           "from" from
                           "thru" thru))
  (unless (<= (syntax-position from) (syntax-position thru))
    (raise-arguments-error 'merge "expected syntax-position of from <= thru"
                           "from" from
                           "thru" thru))
  (define src  (syntax-source from))
  (define line (syntax-line from))
  (define col  (syntax-column from))
  (define pos  (syntax-position from))
  (define end (+ (syntax-position thru) (syntax-span thru)))
  (define span (- end pos))
  (->srcloc-as-list (list src line col pos span)))

(module+ test
  (require rackunit
           racket/match)
  (let ([a #'a]
        [b #'b])
    (match-define (list src line col pos span) (merge a b))
    (check-equal? src (path->string (syntax-source a)))
    (check-equal? line (syntax-line a))
    (check-equal? col (syntax-column a))
    (check-equal? pos (syntax-position a))
    (check-equal? span (- (+ (syntax-position b) (syntax-span b))
                          (syntax-position a))))
  (let ([a #'a]
        [b #'b])
    (match-define (list src line col pos span) (merge a b))
    (check-equal? src (path->string (syntax-source a)))
    (check-equal? line (syntax-line a))
    (check-equal? col (syntax-column a))
    (check-equal? pos (syntax-position a))
    (check-equal? span (- (+ (syntax-position b) (syntax-span b))
                          (syntax-position a))))
  (let ([a #'a]
        [b #'b])
    (define (pos-err? e)
      (and (exn:fail:contract? e)
           (regexp-match? "^merge: expected syntax-position of from <= thru"
                          [exn-message e])))
    (check-exn pos-err?
               (λ () (merge b a)) ;oops: b is later than a
               "appropriate error raised misordered stx positions")))

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

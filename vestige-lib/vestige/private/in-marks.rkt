#lang racket/base

(require racket/match)

(provide in-marks)

(define stop-value (gensym))

(define stop?
  (case-lambda
    [(v) (eq? v stop-value)]         ;fast path
    [vs  (for/and ([v (in-list vs)]) ;general path
           (eq? v stop-value))]))

(define single-stop-value (list stop-value)) ;fast path; preallocate

(define (make-marks-producer cms keys none-v)
  (define stop-values-list
    (match keys
      [(list)   (raise-argument-error 'in-marks "at least one key" keys)]
      [(list _) single-stop-value] ;fast path
      [_        (build-list (length keys) (λ _ stop-value))]))
  (define iter (continuation-mark-set->iterator cms keys none-v))
  (define (produce)
    (define-values (v new-iter) (iter))
    (set! iter new-iter)
    (match v
      [#f              (apply values stop-values-list)]
      [(vector v)      v]                   ;fast path
      [(vector vs ...) (apply values vs)])) ;general path
  produce)

(define (in-marks cms #:none-v [none-v #f] . keys)
  (in-producer (make-marks-producer cms keys none-v) stop?))

(module+ test
  (require rackunit
           syntax/parse/define)
  ;; This little macro applies with-continuation-mark to the identity
  ;; function -- extending the continuation with a new initial frame
  ;; (preventing it being in tail position).
  ;;
  ;; As a result it preserves previously set mark values.
  ;;
  ;; (Because otherwise, if the initial continuation frame already has
  ;; a value for a key, w-c-m replaces the old value with the new
  ;; value in that frame.)
  (define-simple-macro (with-mark! key:expr val:expr body:expr)
    ((λ (x) x) ;add a frame i.e. extend the continuation
     (with-continuation-mark key val body)))

  ;; Although I tried to write `in-marks` to be generally correct, so
  ;; far we only use it for a single key, and to find the first value
  ;; that's a number. Testing specifically that:
  (let ([key 'key])
    (with-mark! key 0
      (with-mark! key 'c
        (with-mark! key 1
          (with-mark! key 'b
            (with-mark! key 'a
             (let ([cms (current-continuation-marks)])
               (check-equal? (continuation-mark-set->list cms key)
                             '(a b 1 c 0))
               (check-equal? (for/list ([v (in-marks cms key)]
                                        #:final (number? v))
                               v)
                             '(a b 1)))))))))

  ;; This is just a shorter alias for with-continuation-mark.
  (define-simple-macro (with-mark key:expr val:expr body:expr)
    (with-continuation-mark key val body))

  ;; A test for multiple keys. Exercises the non-fast path.
  (let ([k1 'k1]
        [k2 'k2])
    (with-mark! k1 0
      (with-mark! k2 'c
        (with-mark k1 1
          (with-mark! k2 'b
            (with-mark! k2 'a
             (let ([cms (current-continuation-marks)])
               (check-equal? (continuation-mark-set->list* cms (list k1 k2))
                             '(#(#f a) #(#f b) #(1 c) #(0 #f)))
               (check-equal? (for/list ([(v1 v2) (in-marks cms k1 k2)]
                                        #:final (and v1 v2))
                               (vector v1 v2))
                             '(#(#f a) #(#f b) #(1 c))))))))))

  ;; Expected failure
  (check-exn exn:fail:contract?
             (λ _ (in-marks (current-continuation-marks)))))

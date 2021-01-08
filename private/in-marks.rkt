#lang racket/base

(require racket/match)

(provide in-marks)

(struct stop ())

(define (make-marks-producer cms keys)
  (define iter (continuation-mark-set->iterator cms keys))
  (define (produce)
    (define-values (v new-iter) (iter))
    (set! iter new-iter)
    (match v
      [#f              (stop)]
      [(vector v)      v]                   ;fast path
      [(vector vs ...) (apply values vs)])) ;general path
  produce)

(define (in-marks cms . keys)
  (in-producer (make-marks-producer cms keys) stop?))

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
  (define-simple-macro (with-mark key:expr val:expr body:expr)
    ((λ (x) x) ;add a frame i.e. extend the continuation
     (with-continuation-mark key val
       body)))
  ;; Although I tried to write `in-marks` to be generally correct, we
  ;; only use it for a single key, and to find the first value that's
  ;; a number. As a result that's the only test here.
  (let ([key 'key])
    (with-mark key 0
      (with-mark key 'c
        (with-mark key 1
          (with-mark key 'b
            (with-mark key 'a
             (let ([cms (current-continuation-marks)])
               (check-equal? (continuation-mark-set->list cms key)
                             '(a b 1 c 0))
               (check-equal? (for/list ([v (in-marks cms key)]
                                        #:final (number? v))
                               v)
                             '(a b 1))))))))))

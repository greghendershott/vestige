#lang racket/base

(require (only-in racket/path file-name-from-path))

(provide syntax-local-infer-name)

;; syntax-local-infer-name is great, except when it can't find a name
;; and synthesizes one from the srcloc, it truncates the pathname to
;; 20 chars, prefixed by "/..." if longer. I find this kind of ugly
;; and distracting.
;;
;; Below is our adaptation with different truncation behavior: When
;; the source is a path, we simply use the filename:line:col.
;;
;; (Although normally I hate discarding full pathnames, we're using
;; this to generate a name where we already have available numerous
;; srclocs about the defined function.)

(define (syntax-local-infer-name stx)
  (let ([prop (simplify-inferred-name (syntax-property stx 'inferred-name))])
    (or (and prop
             (not (void? prop))
             prop)
        (let ([n (and (not (void? prop))
                      (syntax-local-name))])
          (or n
              (let ([s (syntax-source stx)])
                (and s
                     (let ([s (format
                               "~a"
                               (if (path? s)
                                   (path->string (file-name-from-path s))
                                   s))]
                           [l (syntax-line stx)]
                           [c (syntax-column stx)])
                       (if l
                           (string->symbol (format "~a:~a:~a" s l c))
                           (let ([p (syntax-position stx)])
                             (string->symbol (format "~a::~a" s p))))))))))))

(define (simplify-inferred-name name)
  (if (pair? name)
      (let ([name-car (simplify-inferred-name (car name))]
            [name-cdr (simplify-inferred-name (cdr name))])
        (if (eq? name-car name-cdr)
            name-car
            name))
      name))

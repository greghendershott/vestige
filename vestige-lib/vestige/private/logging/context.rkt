#lang racket/base

(require racket/match
         racket/runtime-path
         racket/list
         "srcloc.rkt")

(provide cms->context-srcloc)

;; This will never be as precise as vestige-#%app. Instead, it will be
;; a span somewhere within which is the call site. Although this is
;; not precise enough for e.g. a tool that wants to show the call "in
;; situ" similar to a step debugger, it is useful information. For
;; example it's more than adequate for devops logging purposes, and
;; anyway in such cases vestige-#%app might be avoided for peformance.

(define (cms->context-srcloc cms)
  (for/or ([id+srcloc (in-list (continuation-mark-set->context cms))])
    (define sl (cdr id+srcloc))
    (and sl
         ;; The following dance is to work around the fact that Racket
         ;; structs are generative, and a log receiver might not share
         ;; the same `srcloc` struct as the log event producer.
         (struct? sl)
         (match (vector->list (struct->vector sl))
           [(cons 'struct:srcloc
                  (and sl (list src _line _col _pos _span)))
            ;; Only use srclocs with a complete path, and one to a
            ;; file outside of our implementation.
            (and (path-string? src)
                (complete-path? src)
                (not-our-private-file src)
                (->srcloc-as-list sl))]
           [_ #f]))))

(define-runtime-path here ".")

(define our-private-dir
  (explode-path (simplify-path (build-path here 'up))))

(unless (equal? (build-path "private")
                (car (reverse our-private-dir)))
  (error 'this-collection-top
         "Need to update after reorganizing source file layout"))

(define (not-our-private-file p)
  (not (list-prefix? our-private-dir
                     (explode-path (simplify-path p)))))

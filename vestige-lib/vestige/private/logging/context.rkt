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
    (match id+srcloc
      [(cons _id (and sl (struct* srcloc ([source src]))))
       #:when (and (path-string? src)
                   (complete-path? src)
                   (not (within-this-collection? src)))
       (->srcloc-as-list sl)]
      [_ #f])))

(define-runtime-path here ".")

(define this-collection-top
  (explode-path (simplify-path (build-path here 'up 'up))))

(unless (equal? (build-path "vestige")
                (car (reverse this-collection-top)))
  (error 'this-collection-top
         "Need to update after reorganizing source file layout"))

(define (within-this-collection? p)
  (println p)
  (list-prefix? this-collection-top
                (explode-path (simplify-path p))))

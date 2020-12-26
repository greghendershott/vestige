#lang racket/base

(require racket/match
         "srcloc.rkt")

(provide context-srcloc)

;; This will never be as precise as tracing-#%app. Instead, it will be
;; a span somewhere within which is the call site. Although this is
;; not precise enough for e.g. a tool that wants to show the call "in
;; situ" similar to a step debugger, it is useful information. For
;; example it's more than adequate for devops logging purposes, and
;; anyway in such cases our tracing-#%app might be avoided for
;; peformance.
(define (context-srcloc cms)
  (for/or ([id+srcloc (in-list (continuation-mark-set->context cms))])
    (match id+srcloc
      [(cons _id (and sl (struct* srcloc ([source src]))))
       #:when (and (path-string? src)
                   (complete-path? src)
                   (not (equal? (filename-directory src)
                                (filename-directory (syntax-source #'this-file)))))
       (->srcloc-as-list sl)]
      [_ #f])))

(define (filename-directory p)
  (car (call-with-values (λ () (split-path p)) list)))

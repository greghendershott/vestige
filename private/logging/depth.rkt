#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define)

(provide depth-key ;for use also by tracing/core.rkt
         cms->logging-depth
         marks->logging-depth
         with-more-logging-depth)

;; Key used for a continuation mark to indicate the depth. Non-tail
;; calls of traced functions adjust the depth automatically; see
;; tracing/core.rkt.
;;
;; In addition, `with-more-logging-depth` increases the depth for a
;; dynamic extent.
;;
;; Intentionally not using make-continuation-mark-key.
(define depth-key 'vestige-depth-continuation-mark-key)

(define (cms->logging-depth cms)
  (define marks (continuation-mark-set->list cms depth-key))
  (marks->logging-depth marks))

(define (marks->logging-depth marks)
  (or (findf number? marks)
      0))

(define-syntax-parser with-more-logging-depth
  [(_ e:expr)
   (quasisyntax/loc this-syntax
     (let* ([old-depth (cms->logging-depth (current-continuation-marks))]
            [new-depth (add1 old-depth)])
       (with-continuation-mark depth-key new-depth
         e)))])

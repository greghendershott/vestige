#lang racket/base

(require (for-syntax racket/base)
         syntax/parse/define)

(provide depth-key ;for use also by tracing/wrap.rkt
         with-more-logging-depth
         cms->logging-depth)

;; Key used for a continuation mark to indicate the depth. Non-tail
;; calls of traced functions adjust the depth automatically; see
;; tracing/wrap.rkt.
;;
;; In addition, `with-more-logging-depth` increases the depth for a
;; dynamic extent.
;;
;; Intentionally not using make-continuation-mark-key.
(define depth-key 'vestige-depth-continuation-mark-key)

(define-syntax-parser with-more-logging-depth
  [(_ e:expr)
   (quasisyntax/loc this-syntax
     (let* ([old-depth (cms->logging-depth)]
            [new-depth (add1 old-depth)])
       (with-continuation-mark depth-key new-depth
         e)))])

;; Accepts #f for proc, with the same meaning as for
;; continuation-mark-set-first: An alias for
;; (current-continuation-marks) that can possibly work faster.
(define (cms->logging-depth [cms #f])
  (continuation-mark-set-first cms depth-key 0))

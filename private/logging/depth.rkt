#lang racket/base

(require syntax/parse/define)

(provide depth-key
         cms->logging-depth
         marks->logging-depth
         with-more-logging-depth)

;; Key used for a continuation mark to indicate the depth. Non-tail
;; calls of traced functions adjust the depth automatically; see
;; core.rkt.
;;
;; In addition, `with-more-logging-depth` increases the depth for a
;; dynamic extent.
;;
;; Intentionally not using make-continuation-mark-key because
;; vestige/reciving could be dynamic-required.
(define depth-key 'vestige-depth-continuation-mark-key)

(define (cms->logging-depth cms)
  (define marks (continuation-mark-set->list cms depth-key))
  (marks->logging-depth marks))

(define (marks->logging-depth marks)
  (or (findf number? marks)
      0))

(define-simple-macro (with-more-logging-depth e:expr)
  (let* ([old-depth (cms->logging-depth (current-continuation-marks))]
         [new-depth (add1 old-depth)])
    (with-continuation-mark depth-key new-depth e)))


#lang racket/base

(require syntax/parse/define)

(provide depth-key
         logging-depth
         marks->logging-depth
         call-with-more-logging-depth
         with-more-logging-depth)

;; Key used for a continuation mark to indicate the depth. Non-tail
;; calls of traced functions adjust the depth automatically; see
;; core.rkt.
;;
;; In addition, `with-more-logging-depth` increases the depth for a
;; dynamic extent.
(define depth-key (make-continuation-mark-key 'depth))

(define (logging-depth cms)
  (define marks (continuation-mark-set->list cms depth-key))
  (marks->logging-depth marks))

(define (marks->logging-depth marks)
  (or (findf number? marks)
      0))

(define-simple-macro (with-more-logging-depth body:expr ...+)
  (call-with-more-logging-depth (λ () body ...)))

(define (call-with-more-logging-depth thk)
  (define old-depth (logging-depth (current-continuation-marks)))
  (define new-depth (add1 old-depth))
  (with-continuation-mark depth-key new-depth (thk)))

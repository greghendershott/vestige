#lang racket/base

(require racket/format
         racket/match
         racket/string
         syntax/parse/define
         "../logging/log.rkt"
         "../logging/data.rkt")

(provide log-args
         log-results
         cms->tracing-data
         make-tracing-data)

;;; continuation mark

;; Intentionally not using make-continuation-mark-key.
(define key 'vestige-tracing-continuation-mark-key)

(define-simple-macro (with-tracing-mark data e:expr)
  (with-continuation-mark key data e))

(define (cms->tracing-data cms)
   (continuation-mark-set-first cms key))

(define-simple-macro (log-args e:expr ...)
  (when (log?)
    (do-log-args e ...)))

(define (do-log-args name tail? args kws kw-vals depth
                     caller defn-srcloc formals-srcloc header-srcloc positional-syms)
  (define args-str (string-join
                    (append (match positional-syms
                              [(? symbol? s)
                               (list (~a s "=" (~v args)))]
                              [(? list? ss)
                               (for/list ([s (in-list ss)]
                                          [v (in-list args)])
                                 (~a s "=" (~v v)))])
                            (for/list ([k (in-list kws)]
                                       [v (in-list kw-vals)])
                              (~a k "=" (~v v))))))
  (define prefix (~a "("
                      name
                      (if (equal? args-str "") "" " ")))
  (define suffix ")")
  (define args-from (string-length prefix))
  (define args-upto (+ args-from (string-length args-str)))
  (define message (~a prefix args-str suffix))
  (with-tracing-mark (make-tracing-data #t tail? name message
                                        caller defn-srcloc formals-srcloc header-srcloc
                                        args-from args-upto)
    (with-more-logging-data #:srcloc? #f
      (log! (~a (make-string depth #\>) " " message)))))

(define-simple-macro (log-results e:expr ...)
  (when (log?)
    (do-log-results e ...)))

(define (do-log-results name results depth
                        caller defn-srcloc formals-srcloc header-srcloc)
  (define results-str
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  (with-tracing-mark (make-tracing-data #f #f name results-str
                                        caller defn-srcloc formals-srcloc header-srcloc)
    (with-more-logging-data #:srcloc? #f
      (log! (~a (make-string depth #\<) " " results-str)))))

(define (make-tracing-data call? tail? name message
                           caller defn-srcloc formals-srcloc header-srcloc
                           [args-from #f] [args-upto #f])
  (hasheq 'call       call?
          'tail       tail?
          'name       name
          'message    message
          'args-from  args-from
          'args-upto  args-upto
          'called     (consolidate-definition-srclocs defn-srcloc
                                                      formals-srcloc
                                                      header-srcloc)
          'caller     caller))

;; The srcloc for all three of these should be the same file. If it's
;; ever not, then that's a bug with the srcloc handling in forms.rkt.
;; Supplying it thrice sucks especially because it takes by far the
;; most space. So here we reshape the three srlocs into a little
;; hash-table.
(define (consolidate-definition-srclocs definition formals header)
  (hasheq 'file       (car definition)
          'definition (cdr definition)
          'formals    (cdr formals)
          'header     (cdr header)))

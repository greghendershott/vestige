#lang racket/base

(require racket/format
         racket/match
         racket/string
         syntax/parse/define
         "../logging/app.rkt"
         "../logging/depth.rkt"
         "../logging/log.rkt"
         "../logging/data.rkt")

(provide log-args
         log-results
         cms->tracing-data
         make-called-hash-table)

;; Intentionally not using make-continuation-mark-key.
(define key 'vestige-tracing-continuation-mark-key)

(define-simple-macro (with-tracing-mark data e:expr)
  (with-continuation-mark key data e))

(define (cms->tracing-data cms)
  (match (continuation-mark-set-first cms key)
    [(vector call? tail? name message caller called args-from args-upto)
     (hasheq 'call       call?
             'tail       tail?
             'name       name
             'message    message
             'args-from  args-from
             'args-upto  args-upto
             'called     called
             'caller     (caller-vector->hasheq caller))]
    [#f #f]))

(define (log-args name tail? args kws kw-vals caller called positional-syms)
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
  (with-tracing-mark (vector #t tail? name message caller called args-from args-upto)
    (with-more-logging-data #:srcloc? #f
      (log! (~a (make-string (cms->logging-depth) #\>) " " message)))))

(define (log-results name results caller called)
  (define results-str
    (~a (match results
          [(list)   "#<void>"]
          [(list v) (~v v)]
          [vs       (~s (cons 'values vs))])))
  (with-tracing-mark (vector #f #f name results-str caller called #f #f)
    (with-more-logging-data #:srcloc? #f
      (log! (~a (make-string (cms->logging-depth) #\<) " " results-str)))))

;; The srcloc for all three of these should be the same file. If it's
;; ever not, then that's a bug with the srcloc handling in forms.rkt.
;; Supplying the file pathname thrice sucks especially because it
;; takes by far the most space. So here we reshape the three srclocs
;; into a little hash-table. We state the file pathname, once, and
;; supply only the cdr -- (line col pos span) -- of each
;; srcloc-as-list.
(define (make-called-hash-table definition header formals)
  (match (list definition header formals)
    [(list (cons file definition) (cons file header) (cons file formals))
     (hasheq 'file       file
             'definition definition
             'header     header
             'formals    formals)]
    [_ (raise-arguments-error
        'make-called-hash-table
        "expected srcloc-as-list/c values to have same file"
        "definition" definition
        "header"     header
        "formals"    formals)]))

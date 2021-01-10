#lang racket/base

(require racket/match
         racket/pretty
         (rename-in "private/logging/log.rkt"
                    [topic vestige-topic]
                    [level vestige-level])
         "private/logging/app.rkt"
         "private/logging/context.rkt"
         "private/logging/depth.rkt"
         "private/logging/information.rkt"
         "private/tracing/logging.rkt")

(provide log-receiver-vector->hasheq
         add-presentation-sites
         serializable-hasheq
         vestige-topic
         vestige-level
         ;; Low level instead of using vector->hasheq
         cms->logging-depth
         cms->logging-info
         cms->caller-srcloc
         cms->context-srcloc
         cms->tracing-data
         performance-vectors->hasheq)

(define (log-receiver-vector->hasheq v)
  (match v
    [(vector level message (? continuation-mark-set? cms) topic)
     (define tracing (cms->tracing-data cms))
     (hasheq 'message (or (and tracing (hash-ref tracing 'message #f))
                          message)
             'topic   topic
             'level   level
             'depth   (cms->logging-depth cms)
             'caller  (cms->caller-srcloc cms)
             'context (cms->context-srcloc cms)
             'info    (cms->logging-info cms)
             'tracing tracing)]
    [(vector level message _unknown-data topic)
     (hasheq 'message message
             'topic   topic
             'level   level
             'depth   0)]))

;; Sorts through the various cases of collected data from tracing or
;; with-more-loging-info, to add primary-site and secondary-site
;; mappings. i.e. When the user wants to see sites associated with a
;; logging event, here is a recommendation what to show, where, and
;; how.
;;
;; Although I wasn't originally sure if this belongs down here in this
;; library, I was persuaded by many iterations of fussy/tricky code up
;; in Racket Mode's Emacs front-end.
;;
;; On the one hand, I don't want to predetermine the UI for all tools.
;; On the other hand, this kind of presentation logic is not
;; necessarily immediately obvious from the raw collected data. If a
;; tool wants a different presentation, at least this source code is
;; available as an example, and they still have access to all the data
;; from log-receiver-vector->hasheq.
(define (add-presentation-sites ht)
  (define (add #:primary primary #:secondary secondary)
    (hash-set* ht
               'primary-site primary
               'secondary-site secondary))
  (define (add-tracing ht)
    (match (hash-ref ht 'tracing #f)
    [(hash-table ['call         call?]
                 ['message      message]
                 ['args-from    args-from]
                 ['args-upto    args-upto]
                 ['formals      formals]
                 ['header       header])
     ;; Tracing call or results. The primary site is the called site
     ;; i.e where the function is defined. The secondary site is the
     ;; caller site (if any such information is available).
     (if call?
         (add #:primary
              (match formals
                [(list file _line _col pos span)
                 (cond
                   ;; Empty formals span (thunk); no actual args
                   ;; and anyway no place to show them: Instead
                   ;; highlight the header keeping its existing
                   ;; text.
                   [(or (zero? span)
                        (equal? args-from args-upto))
                    (match-let ([(list file _line _col pos span) header])
                      (list 'highlight file pos (+ pos span)))]
                   ;; Non-empty formals span: replace the text at
                   ;; formals location with the actual arguments.
                   ;; These are a substring of the message.
                   [else
                    (list 'replace file pos (+ pos span)
                          (substring message args-from args-upto))])])
              #:secondary
              (match (hash-ref ht 'caller #f)
                [(list file _line _col pos span)
                 (list 'replace file pos (+ pos span) message)]
                [_ #f]))
         (let ([msg (string-append "⇒ " message)])
           (add #:primary
                (match header
                  [(list file _line _col pos span)
                   (list 'after file pos (+ pos span) msg)]
                  [_ #f])
                #:secondary
                (match (hash-ref ht 'caller #f)
                  [(list file _line _col pos span)
                   (list 'after file pos (+ pos span) msg)]
                  [_ #f]))))]
    [_ #f]))
  (define (add-info ht)
     ;; Non-tracing logging that has srcloc arising from
     ;; with-more-logging-info, has that as its primary site. There is
     ;; no secondary site.
     (match (hash-ref ht 'info #f)
       [(hash-table ['srcloc (list file _line _col pos span)])
        (add #:primary   (list 'highlight file pos (+ pos span))
             #:secondary #f)]
       [_ #f]))
  (or (add-tracing ht) ;most specific, try first
      (add-info ht)
      ;; Otherwise, nothing actionable wrt showing sites. A tool could
      ;; should show context srcloc, but: 1. That's a stand-alone
      ;; property that either is present or not -- there is no
      ;; complicating presentation logic like the above. 2. Context
      ;; can be a very large span -- benefitting from a presentation
      ;; other than boldly highlghting the entire thing.
      ht))

;; Change as necessary to satisfy `jsexpr?`
(define (serializable-hasheq h)
  (define (serialize-key k)
    (match k
      [(? symbol?) k]
      [_ (string->symbol (format "~a" k))]))
  (define (serialize-value v)
    (match v
      [(? hash?)   (serializable-hasheq v)]
      [(? list?)   (map serialize-value v)]
      [(? vector?) (map serialize-value (vector->list v))]
      [(? thread?) (format "~a" (object-name v))]
      [(or (? boolean?)
           (? string?)
           (? exact-integer?)
           (and (? inexact-real?)
                (? rational?)))
       v]
      [_ (format "~a" v)]))
  (for/hasheq ([(k v) (in-hash h)])
    (values (serialize-key k)
            (serialize-value v))))

;; For use by things like example.rkt.
(module+ private
  (provide start stop)

  ;; Starts a log receiver thread that pretty-prints
  ;; log-receiver-vector->hasheq to an output-string.
  (define (start)
    (define receiver (make-log-receiver (current-logger)
                                        vestige-level vestige-topic
                                        'info 'example
                                        'fatal #f))
    (define out (open-output-string))
    (define (prn v)
      (pretty-print (log-receiver-vector->hasheq v)
                    out))
    (define (clear)
      (define v (sync/timeout 0 receiver))
      (when v
        (prn v)
        (clear)))
    (define stop-chan (make-channel))
    (define thd
      (thread
       (λ ()
         (let loop ()
           (match (sync receiver stop-chan)
             ['stop (clear)]
             [v (prn v) (loop)])))))
    (set! stop
          (λ ()
            (channel-put stop-chan 'stop)
            (thread-wait thd)
            (display (get-output-string out)))))

  ;; Stop the log receiver thread and the accumulated output.
  (define (stop)
    (error 'stop-log-receiver-thread "not started"))

  (module+ start
    (start))

  (module+ stop
    (stop)))

#lang racket/base

(require racket/class)
(provide (except-out (all-from-out racket/class)
                     define/public
                     define/override))

(require "private/class.rkt")
(provide (rename-out [trace-define/public define/public]
                     [trace-define/override define/override]))

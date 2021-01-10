#lang racket/base

(require racket/class)
(provide (except-out (all-from-out racket/class)
                     define/private
                     define/public
                     define/pubment
                     define/override
                     define/overment
                     define/augride
                     define/augment
                     define/public-final
                     define/override-final
                     define/augment-final))

(require "../../private/tracing/class.rkt")
(provide (rename-out
          [trace-define/private        define/private]
          [trace-define/public         define/public]
          [trace-define/pubment        define/pubment]
          [trace-define/override       define/override]
          [trace-define/overment       define/overment]
          [trace-define/augride        define/augride]
          [trace-define/augment        define/augment]
          [trace-define/public-final   define/public-final]
          [trace-define/override-final define/override-final]
          [trace-define/augment-final  define/augment-final]))

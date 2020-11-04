#lang info

(define collection 'use-pkg-name)
(define deps '(["base" #:version "7.8"]))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define test-omit-paths '("example/"))
(define scribblings '(("vestige.scrbl")))

#lang info

(define collection 'multi)
(define deps '(["base" #:version "7.8"]))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define test-omit-paths '("vestige/example/"))
(define clean '("compiled" "vestige/doc" "vestige/doc/vestige"))

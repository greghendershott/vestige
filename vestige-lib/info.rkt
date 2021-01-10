#lang info

(define collection 'multi)
(define deps '(["base" #:version "7.8"]))
(define build-deps '("rackunit-lib"))
(define test-omit-paths '("vestige/example/"))
(define clean '("compiled"))
(define pkg-desc "implementation part of \"vestige\"")

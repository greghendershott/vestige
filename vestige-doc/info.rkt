#lang info

(define collection 'multi)
(define deps '(["base" #:version "7.8"]))
(define build-deps '("vestige-lib"
                     "racket-doc"
                     "scribble-lib"))
(define clean '("compiled" "vestige/doc" "vestige/doc/vestige"))
(define update-implies '("vestige-lib"))
(define pkg-desc "documentation part of \"vestige\"")

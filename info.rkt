#lang info

(define collection "irandom")

(define version "0.5")

(define pkg-desc
  "Racket implementation of Bob Jenkins' ISAAC pseudorandom number generator (Indirection, Shift, Accumulate, Add, and Count).")

(define deps '("base"))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/irandom.scrbl" ())))

(define compile-omit-paths '("test"))

(define test-omit-paths '("main.rkt" "info.rkt" "test/data" "scribblings" "doc" "compiled"
                                     "isaac.rkt" "uuid.rkt"))
(define test-include-paths '("test/test.rkt"))

(define pkg-authors '(hkrish))

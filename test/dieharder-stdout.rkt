#lang racket/base


;;----------------------------------------
;; Output random 32bit integers to a file irand_test.txt in current directory. This can be
;; used to run the battery of tests in the "Direharder random number test suite"
;; (https://webhome.phy.duke.edu/~rgb/General/dieharder.php)
(module+ main
  (require "../isaac.rkt")
  (parameterize ([current-irandom-context (make-irandom-context)])
    (let loop ()
      (display (irandom-bytes 4))
      (loop))))

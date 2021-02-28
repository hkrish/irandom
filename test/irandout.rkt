#lang racket/base


;;----------------------------------------
;; Output random 32bit integers to a file irand_test.txt in current directory. This can be
;; used to run the battery of tests in the "Direharder random number test suite"
;; (https://webhome.phy.duke.edu/~rgb/General/dieharder.php)
(module+ main
  (require racket/cmdline
           racket/fixnum
           racket/format
           "../isaac.rkt")

  (define filename (make-parameter "./irand_test.txt"))
  (define N (make-parameter 100000))
  (define null-seed? (make-parameter #f))
  (define ascii? (make-parameter #f))
  (define jenkins? (make-parameter #f))

  (command-line
   #:once-each
   [("-o" "--output") fname "Output file name (Default ./irand_test.txt)"
                      (filename fname)]
   [("-c" "--count") cnt "Number of 32bit values to output (Default 10000)"
                     (N (inexact->exact (string->number cnt)))]
   [("-a" "--ascii") "ASCII output format (Default false)"
                     (ascii? #t)]
   [("-n" "--null") "Seed ISAAC with zeros? (Default false)"
                    (null-seed? #t)]
   [("-j" "--jenkins") "Generate the ISAAC test file (Default false)"
                       (jenkins? #t)])

  (with-output-to-file (filename) #:exists 'replace
    (lambda ()
      (parameterize ([current-irandom-context (if (null-seed?)
                                                  (make-irandom-null-context)
                                                  (make-irandom-context))])
        (cond
          [(jenkins?)
           (parameterize ([current-irandom-context (make-irandom-null-context)])
             (for ([i (in-range 2)])
               (generate-next-random-block! (current-irandom-context))
               (for ([j (in-range 256)])
                 (display (~r (fxvector-ref (irandom-context-randrsl
                                             (current-irandom-context)) j)
                              #:base 16 #:min-width 8 #:pad-string "0"))
                 (when (= 7 (fxand j 7)) (newline)))))]
          [(ascii?)
           (displayln "type: d")
           (displayln (format "count: ~a" (N)))
           (displayln "numbit: 32")
           (for ([i (in-range (N))])
             (displayln (irandom-32)))]
          [else
           (let* ([NN (* 4 (N))]
                  [nbs (let loop ([nbs 256]) (if (= 0 (remainder NN nbs)) nbs (loop (/ nbs 2))))]
                  [NN (fxquotient NN nbs)])
             (for ([i (in-range NN)])
               (display (irandom-bytes nbs))))])))))

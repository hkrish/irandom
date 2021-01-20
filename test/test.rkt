#lang racket/base

(require racket/file
         racket/fixnum
         racket/format
         (only-in racket/format
                  ~a)
         racket/port
         racket/string
         rackunit
         "../isaac.rkt"
         "../uuid.rkt")

;; Check output (with a null context) with ./randvect.txt
(let ([bobs-numbers (file->lines "./data/randvect.txt")]
      [my-numbers
       (string-split
        (with-output-to-string
          (lambda ()
            (parameterize ([current-irandom-context (make-irandom-null-context)])
              (for ([i (in-range 2)])
                (generate-next-random-block! (current-irandom-context))
                (for ([j (in-range 256)])
                  (display (~r (fxvector-ref (irandom-context-randrsl
                                              (current-irandom-context)) j)
                               #:base 16 #:min-width 8 #:pad-string "0"))
                  (when (= 7 (fxand j 7)) (newline))))))))])
  (check-equal? bobs-numbers my-numbers))


(for ([p (in-directory "./data/seeded" (lambda (_) #f))])
  (let ([bobs-numbers (with-input-from-file p read)])
    (when (and (list? bobs-numbers)
               (andmap list? bobs-numbers)
               (= 2 (length bobs-numbers))
               (eq? 'seed (caar bobs-numbers))
               (= 257 (length (car bobs-numbers))))
      (let* ([seeds (cdar bobs-numbers)]
             [bobs-numbers (cadr bobs-numbers)]
             [count (length bobs-numbers)]
             [my-numbers
              (parameterize ([current-irandom-context (make-irandom-null-context #:seeds seeds)])
                (for/list ([i (in-range count)]) (irandom-32)))])
        (check-equal? bobs-numbers my-numbers)))))


(check-true (andmap fixnum? (build-list 1000000 (lambda (_) (irandom-fixnum)))))

(check-true (andmap uuid-string? (for/list ([i (in-range 100000)]) (uuid-string))))


(define (hex o)
  (cond
    [(flonum? o) (hex (real->floating-point-bytes o (if (single-flonum? o) 4 8) #t))]
    [(number? o) (display (format "~x" o))]
    [else (for ([v o]) (display (format "~x " v)))]))

(define (grp n)
  (string-join
   (map list->string
        (cdr (foldr (lambda (e l)
                      (if (>= (car l) 3)
                          (list* 0 (list e) (cdr l))
                          (list* (add1 (car l)) (cons e (cadr l)) (cddr l))))
                    '(0 ()) (string->list (number->string n))))) ","))

(let* ([N32 10000000]
       [N08 (* 4 N32)]
       [N64 (inexact->exact (/ N32 2))])
  (displayln (format "[Performance test] : ~a 32bit random values" (grp N32)))
  (display (~a "  irandom-bytes :" #:width 25))
  (time (and #t (irandom-bytes N08)))
  (display (~a "  irandom-fxvector-32 :" #:width 25))
  (time (and #t (irandom-fxvector-32 N32)))
  (display (~a "  irandom-list-32 :" #:width 25))
  (time (and #t (irandom-list-32 N32)))
  (display (~a "  irandom-32 :" #:width 25))
  (time (and #t (for ([i (in-range N32)]) (irandom-32))))
  (display (~a "  irandom-fixnum :" #:width 25))
  (time (and #t (for ([i (in-range N64)]) (irandom-fixnum))))
  (display (~a "  irandom-fxvector :" #:width 25))
  (time (and #t (irandom-fxvector N64)))
  (display (~a "  irandom :" #:width 25))
  (time (and #t (for ([i (in-range N64)]) (irandom))))
  (display (~a "  irandom-flvector :" #:width 25))
  (time (and #t (irandom-flvector N64)))
  (display (~a "  racket/random :" #:width 25))
  (time (and #t (for ([i (in-range N64)]) (random))))
  (void)
  )


;;----------------------------------------
;; If main module is run, output random 32bit integers to a file irand_test.txt in current
;; directory. This can be used to run the battery of tests in the "Direharder random
;; number test suite" (https://webhome.phy.duke.edu/~rgb/General/dieharder.php)
(module+ main
  (require racket/cmdline)
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
               (display (irandom-bytes nbs))))]))))
  )

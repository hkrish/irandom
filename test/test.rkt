#lang racket/base

(module+ test
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

  (check-true (andmap uuid-string? (for/list ([i (in-range 100000)]) (uuid-bytes))))
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
                        (if (>= (car l) 2)
                            (list* 0 (list e) (cdr l))
                            (list* (add1 (car l)) (cons e (cadr l)) (cddr l))))
                      '(-1 ()) (string->list (number->string n)))))
     ","))

  (let* ([N32 10000000]
         [N08 (* 4 N32)]
         [N64 (inexact->exact (/ N32 2))]
         [NUUID 1000000])
    (collect-garbage)
    (displayln (format "[Performance test] : ~a 32bit random values" (grp N32)))

    (display (~a "  irandom-bytes :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-bytes N08)))

    (display (~a "  irandom-fxvector-32 :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-fxvector-32 N32)))

    (display (~a "  irandom-list-32 :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-list-32 N32)))

    (display (~a "  irandom-32 :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (for ([i (in-range N32)]) (irandom-32))))

    (display (~a "  irandom-fixnum :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (for ([i (in-range N64)]) (irandom-fixnum))))

    (display (~a "  irandom-fxvector :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-fxvector N64)))

    (display (~a "  irandom-flvector :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-flvector N64)))

    (display (~a "  irandom-u32vector :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-u32vector N32)))

    (display (~a "  irandom-u64vector :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-u64vector N64)))

    (display (~a "  irandom-f64vector :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (irandom-f64vector N64)))

    (display (~a "  irandom :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (for ([i (in-range N64)]) (irandom))))

    (display (~a "  racket/random :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (for ([i (in-range N64)]) (random))))

    (displayln (format "[Performance test] : ~a UUIDs" (grp NUUID)))

    (display (~a "  uuid-bytes :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (for ([i (in-range NUUID)]) (uuid-bytes))))

    (display (~a "  uuid-string :" #:width 25))
    (collect-garbage 'minor)
    (time (and #t (for ([i (in-range NUUID)]) (uuid-string))))

    (void))
  )

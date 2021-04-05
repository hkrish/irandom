#lang racket/base

;; ----------------------------------------
;; References:
;;
;; ISAAC: a fast cryptographic random number generator
;;  - http://burtleburtle.net/bob/rand/isaacafa.html
;;  - Source license: Public domain.
;;
;; Doug Hoyte's Common Lisp port
;;  - https://hcsw.org/downloads/isaac.lisp
;;  - isaac.lisp (C) May 2008 Doug Hoyte, HCSW
;;  - Source license: BSD. you can do anything you want with it (but no warranty).
;;
;; ----------------------------------------


(require (for-syntax racket/base racket/syntax)
         (only-in racket/require filtered-in)
         (only-in racket/random crypto-random-bytes)
         (only-in racket/fixnum fxvector? make-fxvector for/fxvector)
         (only-in racket/flonum make-flvector)
         (only-in ffi/vector
                  make-u32vector
                  u32vector-ref
                  u32vector-set!
                  u32vector->cpointer
                  make-u64vector
                  u64vector-ref
                  u64vector-set!
                  u64vector->cpointer
                  make-f64vector)
         (only-in racket/unsafe/ops
                  unsafe-struct*-ref
                  unsafe-struct*-set!
                  unsafe-bytes-ref
                  unsafe-bytes-set!)
         (only-in ffi/unsafe memcpy _byte _uint32 _uint64)
         (filtered-in
          (lambda (name)
            (and (or (regexp-match #rx"^unsafe-fx" name)
                     (regexp-match #rx"^unsafe-fl" name)
                     (regexp-match #rx"^unsafe-f64" name))
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops))

(provide current-irandom-context
         irandom-context?
         make-irandom-context
         irandom-bytes
         irandom-32
         irandom-fxvector-32
         irandom-list-32
         irandom-fixnum
         irandom
         irandom-fxvector
         irandom-flvector
         irandom-u32vector
         irandom-u64vector
         irandom-f64vector
         ;; Private: Only for testing
         make-irandom-null-context
         irandom-context-randrsl
         generate-next-random-block!)


;; Context of random number generator
(struct irandom-context ([randcnt #:mutable] ; 4 bytes
                         [randrsl]           ; 256 x 4 bytes
                         [randmem]           ; 256 x 4 bytes
                         [randa #:mutable]   ; 4 bytes
                         [randb #:mutable]   ; 4 bytes
                         [randc #:mutable])  ; 4 bytes
  #:authentic)


(define-syntax-rule (randcnt ctx) (unsafe-struct*-ref ctx 0))
(define-syntax-rule (set-randcnt! ctx v) (unsafe-struct*-set! ctx 0 v))
(define-syntax-rule (randrsl ctx) (unsafe-struct*-ref ctx 1))
(define-syntax-rule (randmem ctx) (unsafe-struct*-ref ctx 2))
(define-syntax-rule (randa ctx) (unsafe-struct*-ref ctx 3))
(define-syntax-rule (set-randa! ctx v) (unsafe-struct*-set! ctx 3 v))
(define-syntax-rule (randb ctx) (unsafe-struct*-ref ctx 4))
(define-syntax-rule (set-randb! ctx v) (unsafe-struct*-set! ctx 4 v))
(define-syntax-rule (randc ctx) (unsafe-struct*-ref ctx 5))
(define-syntax-rule (set-randc! ctx v) (unsafe-struct*-set! ctx 5 v))


(define-syntax-rule (rsh a b) (fxrshift (fxand a #xFFFFFFFF) b))
(define-syntax-rule (lsh a b) (fxand #xFFFFFFFF (fxlshift a b)))
(define-syntax fx+/32
  (syntax-rules ()
    [(_ a b) (fxand #xFFFFFFFF (fx+ a b))]
    [(_ a b c) (fxand #xFFFFFFFF (fx+ a b c))]))
(define-syntax-rule (ind a) (fxand a #xFF))


(define (generate-next-random-block! ctx)
  (let* ([m (randmem ctx)]
         [r (randrsl ctx)]
         [c (fx+ 1 (randc ctx))])
    (let-values
        ([(a b)
          (for/fold ([a (randa ctx)] [b (fx+ (randb ctx) c)])
                    ([i (in-range 256)])
            (let* ([a (fxxor
                       a
                       (let ([ii (fxand i 3)])
                         (cond
                           [(fx= ii 0) (lsh a 13)]
                           [(fx= ii 1) (rsh a 6)]
                           [(fx= ii 2) (lsh a 2)]
                           [else (rsh a 16)])))]
                   [a (fx+/32 (fxvector-ref m (ind (fx+ i 128))) a)]
                   [x (fxvector-ref m i)]
                   [y (fx+/32 (fxvector-ref m (ind (fxrshift x 2))) a b)])
              (fxvector-set! m i y)
              (let ([b (fx+/32 (fxvector-ref m (ind (fxrshift y 10))) x)])
                (fxvector-set! r i b)
                (values a b))))])
      (set-randa! ctx a)
      (set-randb! ctx b)
      (set-randc! ctx c)
      ctx)))


(define-syntax mix*
  (syntax-rules ()
    [(_ body) body]
    [(_ (a b c d (sf s)) forms ... body)
     (let* ([a (fxxor a (sf b s))]
            [d (fx+/32 d a)] [b (fx+/32 b c)])
       (mix* forms ... body))]))


(define (scramble! ctx)
  (define (mix a b c d e f g h)
    (mix* (a b c d (lsh 11)) (b c d e (rsh 2))
          (c d e f (lsh 8)) (d e f g (rsh 16))
          (e f g h (lsh 10)) (f g h a (rsh 4))
          (g h a b (lsh 8)) (h a b c (rsh 9))
          (values a b c d e f g h)))
  (let*-values
      ([(r) (randrsl ctx)]
       [(m) (randmem ctx)]
       [(a b c d e f g h)
        (for/fold ([a #x9e3779b9] [b #x9e3779b9] [c #x9e3779b9]
                                  [d #x9e3779b9] [e #x9e3779b9] [f #x9e3779b9]
                                  [g #x9e3779b9] [h #x9e3779b9])
                  ([i (in-range 4)])
          (mix a b c d e f g h))]
       [(a b c d e f g h)
        (for/fold ([a a] [b b] [c c] [d d] [e e] [f f] [g g] [h h])
                  ([i (in-range 0 256 8)])
          (let*-values ([(a) (fx+/32 a (fxvector-ref r i))]
                        [(b) (fx+/32 b (fxvector-ref r (fx+ i 1)))]
                        [(c) (fx+/32 c (fxvector-ref r (fx+ i 2)))]
                        [(d) (fx+/32 d (fxvector-ref r (fx+ i 3)))]
                        [(e) (fx+/32 e (fxvector-ref r (fx+ i 4)))]
                        [(f) (fx+/32 f (fxvector-ref r (fx+ i 5)))]
                        [(g) (fx+/32 g (fxvector-ref r (fx+ i 6)))]
                        [(h) (fx+/32 h (fxvector-ref r (fx+ i 7)))]
                        [(a b c d e f g h) (mix a b c d e f g h)])
            (fxvector-set! m i a)
            (fxvector-set! m (fx+ i 1) b)
            (fxvector-set! m (fx+ i 2) c)
            (fxvector-set! m (fx+ i 3) d)
            (fxvector-set! m (fx+ i 4) e)
            (fxvector-set! m (fx+ i 5) f)
            (fxvector-set! m (fx+ i 6) g)
            (fxvector-set! m (fx+ i 7) h)
            (values a b c d e f g h)))])
    ;; Do a second pass to make all of the seed affect all of m
    (for/fold ([a a] [b b] [c c] [d d] [e e] [f f] [g g] [h h])
              ([i (in-range 0 256 8)])
      (let*-values ([(a) (fx+/32 a (fxvector-ref m i))]
                    [(b) (fx+/32 b (fxvector-ref m (fx+ i 1)))]
                    [(c) (fx+/32 c (fxvector-ref m (fx+ i 2)))]
                    [(d) (fx+/32 d (fxvector-ref m (fx+ i 3)))]
                    [(e) (fx+/32 e (fxvector-ref m (fx+ i 4)))]
                    [(f) (fx+/32 f (fxvector-ref m (fx+ i 5)))]
                    [(g) (fx+/32 g (fxvector-ref m (fx+ i 6)))]
                    [(h) (fx+/32 h (fxvector-ref m (fx+ i 7)))]
                    [(a b c d e f g h) (mix a b c d e f g h)])
        (fxvector-set! m i a)
        (fxvector-set! m (fx+ i 1) b)
        (fxvector-set! m (fx+ i 2) c)
        (fxvector-set! m (fx+ i 3) d)
        (fxvector-set! m (fx+ i 4) e)
        (fxvector-set! m (fx+ i 5) f)
        (fxvector-set! m (fx+ i 6) g)
        (fxvector-set! m (fx+ i 7) h)
        (values a b c d e f g h)))
    (generate-next-random-block! ctx)
    (set-randcnt! ctx 256)
    ctx))


(define (make-irandom-null-context #:seeds [seeds #f])
  (let* ([seeds (or seeds (make-fxvector 256 0))]
         [seeds (if (fxvector? seeds) seeds (for/fxvector ([s (in-list seeds)]) s))])
    (scramble! (irandom-context 0 seeds (make-fxvector 256 0) 0 0 0))))


(define (make-irandom-context)
  (let ([bs (crypto-random-bytes 1024)]
        [bige? (system-big-endian?)])
    (scramble!
     (irandom-context
      0
      (for/fold ([vec (make-fxvector 256 0)])
                ([i (in-range 0 256)])
        (let ([j (fx* i 4)])
          (fxvector-set!
           vec i (fxand #xFFFFFFFF (integer-bytes->integer bs #f bige? j (fx+ j 4))))
          vec))
      (make-fxvector 256 0)
      0 0 0))))


(define current-irandom-context (make-parameter (make-irandom-context)))


(define-syntax-rule (check-length who n)
  (unless (and (fixnum? n) (>= n 0))
    (raise-argument-error who "(and/c fixnum? nonnegative?)" n)))


(define (irandom-bytes/unchecked n)
  (let ([dest (make-bytes n)])
    (cond
      [(fx= n 0) dest]
      [else
       (let* ([ctx (current-irandom-context)]
              [rsl (randrsl ctx)]
              [cnt (randcnt ctx)]
              [n4 (fx* (fxquotient n 4) 4)])
         (let loop ([i 0] [cnt cnt])
           (cond
             [(fx<= cnt 0)
              (generate-next-random-block! ctx)
              (loop i 256)]
             [(fx>= i n4)
              (if (fx> n n4)
                  (let* ([cnt (fx- cnt 1)]
                         [rnd (fxand #xFFFFFFFF (fxvector-ref rsl cnt))]
                         [l (fx- n n4)])
                    (set-randcnt! ctx cnt)
                    (when (fx> l 0) (unsafe-bytes-set! dest i (fxand #xFF rnd)))
                    (when (fx> l 1)
                      (unsafe-bytes-set! dest (fx+ i 1) (fxand #xFF (fxrshift rnd 8))))
                    (when (fx> l 2)
                      (unsafe-bytes-set! dest (fx+ i 2) (fxand #xFF (fxrshift rnd 16)))))
                  (set-randcnt! ctx cnt))
              dest]
             [else
              (let* ([cnt (fx- cnt 1)]
                     [rnd (fxand #xFFFFFFFF (fxvector-ref rsl cnt))])
                (unsafe-bytes-set! dest i (fxand #xFF rnd))
                (unsafe-bytes-set! dest (fx+ i 1) (fxand #xFF (fxrshift rnd 8)))
                (unsafe-bytes-set! dest (fx+ i 2) (fxand #xFF (fxrshift rnd 16)))
                (unsafe-bytes-set! dest (fx+ i 3) (fxrshift rnd 24))
                (loop (fx+ i 4) cnt))])))])))


(define (irandom-bytes n)
  (check-length 'irandom-bytes n)
  (irandom-bytes/unchecked n))


(define (irandom-32)
  (let* ([ctx (current-irandom-context)]
         [cnt (fx- (randcnt ctx) 1)])
    (cond
      [(fx>= cnt 0)
       (set-randcnt! ctx cnt)
       (fxvector-ref (randrsl ctx) cnt)]
      [else
       (generate-next-random-block! ctx)
       (set-randcnt! ctx 255)
       (fxvector-ref (randrsl ctx) 255)])))


(define (irandom-fxvector-32/unchecked n)
  (let ([dest (make-fxvector n)])
    (cond
      [(fx= n 0) dest]
      [else
       (let* ([ctx (current-irandom-context)]
              [rsl (randrsl ctx)]
              [cnt (randcnt ctx)])
         (let loop ([i 0] [cnt cnt])
           (cond
             [(fx<= cnt 0)
              (generate-next-random-block! ctx)
              (loop i 256)]
             [(fx>= i n) (set-randcnt! ctx cnt) dest]
             [else
              (let ([cnt (fx- cnt 1)])
                (fxvector-set! dest i (fxvector-ref rsl cnt))
                (loop (fx+ i 1) cnt))])))])))


(define (irandom-fxvector-32 n)
  (check-length 'irandom-fxvector-32 n)
  (irandom-fxvector-32/unchecked n))


(define (irandom-u32vector n)
  (check-length 'irandom-u32vector n)
  (let ([out (make-u32vector n)]
        [in (irandom-bytes/unchecked (fx* n 4))])
    (memcpy (u32vector->cpointer out) 0 in 0 n _uint32)
    out))


(define (irandom-list-32 n)
  (check-length 'irandom-list-32 n)
  (let ([v (irandom-fxvector-32/unchecked n)])
    (let loop ([i 0] [dst null])
      (cond
        [(fx= i n) dst]
        [else (loop (fx+ i 1) (cons (fxvector-ref v i) dst))]))))


(define (irandom-fixnum)
  (let* ([v (irandom-fxvector-32/unchecked 2)]
         [a (fxvector-ref v 0)]
         [b (fxvector-ref v 1)])
    (fxior (fxlshift a 28) (fxand #xfffffff b))))


(define (irandom)
  (let* ([v (irandom-fxvector-32/unchecked 2)]
         [a (fxvector-ref v 0)]
         [b (fxvector-ref v 1)])
    (fl* 8.673617379884035e-19   ; (expt 2 -60)
         (real->double-flonum (fxior (fxlshift a 28) (fxand #xfffffff b))))))


(define (irandom-u64vector n)
  (check-length 'irandom-u64vector n)
  (let ([out (make-u64vector n)]
        [in (irandom-bytes/unchecked (fx* n 8))])
    (memcpy (u64vector->cpointer out) 0 in 0 n _uint64)
    out))


(define-syntax (-vector-loop stx)
  (syntax-case stx ()
    [(_ typ dst src n offset combine rest-expr)
     #'(-vector-loop fxvector typ dst src n offset combine rest-expr)]
    [(_ src-typ typ dst src n offset combine rest-expr)
     (with-syntax ([src-ref (format-id #'typ "~a-ref" #'src-typ)]
                   [vec-set! (format-id #'typ "~a-set!" #'typ)])
       #'(let loop ([i (fx- n 1)])
           (cond
             [(fx< i 0) rest-expr]
             [else
              (let ([a (src-ref src (fx* i 2))]
                    [b (src-ref src (fx+ (fx* i 2) 1))])
                (vec-set! dst (fx+ i offset) (combine a b))
                (loop (fx- i 1)))])))]))


(define-syntax-rule (-fx-combine a b)
  (fxior (fxlshift a 28) (fxand #xfffffff b)))


(define (irandom-fxvector n)
  (check-length 'irandom-fxvector n)
  (let ([out (make-fxvector n)]
        [in (irandom-fxvector-32/unchecked (fx* n 2))])
    (-vector-loop fxvector out in n 0 -fx-combine out)))


(define-syntax-rule (-fl-combine a b)
  (fl* 8.673617379884035e-19 ; (expt 2 -60)
       (real->double-flonum (fxior (fxlshift a 28) (fxand #xfffffff b)))))


(define (irandom-flvector n)
  (check-length 'irandom-flvector n)
  (let ([out (make-flvector n)]
        [in (irandom-fxvector-32/unchecked (fx* n 2))])
    (-vector-loop flvector out in n 0 -fl-combine out)))


(define (irandom-f64vector n)
  (check-length 'irandom-f64vector n)
  (let ([out (make-f64vector n)]
        [in (irandom-fxvector-32/unchecked (fx* n 2))])
    (-vector-loop f64vector out in n 0 -fl-combine out)))

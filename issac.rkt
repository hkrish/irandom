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

(require racket/fixnum
         (only-in racket/random
                  crypto-random-bytes)
         racket/unsafe/ops)


(provide (rename-out [irand-ctx? irandom-context?])
         current-irandom-context
         irandom-init/null
         irandom-init
         irandom-32
         irand-ctx-randrsl              ; Private: Only for testing
         generate-next-random-block!    ; Private: Only for testing
         )


;; Context of random number generator
(struct irand-ctx ([randcnt #:mutable]  ; 4 bytes
                   [randrsl]            ; 256 x 4 bytes
                   [randmem]            ; 256 x 4 bytes
                   [randa #:mutable]    ; 4 bytes
                   [randb #:mutable]    ; 4 bytes
                   [randc #:mutable])   ; 4 bytes
  #:transparent #:authentic)


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


(define-syntax-rule (rsh a b) (unsafe-fxrshift (unsafe-fxand a #xFFFFFFFF) b))
(define-syntax-rule (lsh a b) (unsafe-fxand #xFFFFFFFF (unsafe-fxlshift a b)))
(define-syntax fx+/32
  (syntax-rules ()
    [(_ a b) (unsafe-fxand #xFFFFFFFF (unsafe-fx+ a b))]
    [(_ a b c) (unsafe-fxand #xFFFFFFFF (unsafe-fx+ a b c))]))
(define-syntax-rule (ind a) (unsafe-fxand a #xFF))


(define (generate-next-random-block! ctx)
  (let* ([m (randmem ctx)]
         [r (randrsl ctx)]
         [c (unsafe-fx+ 1 (randc ctx))])
    (let-values
        ([(a b)
          (for/fold ([a (randa ctx)] [b (unsafe-fx+ (randb ctx) c)])
                    ([i (in-range 256)])
            (let* ([a (unsafe-fxxor
                       a
                       (let ([ii (unsafe-fxand i 3)])
                         (cond
                           [(fx= ii 0) (lsh a 13)]
                           [(fx= ii 1) (rsh a 6)]
                           [(fx= ii 2) (lsh a 2)]
                           [else (rsh a 16)])))]
                   [a (fx+/32 (unsafe-fxvector-ref m (ind (unsafe-fx+ i 128))) a)]
                   [x (fxvector-ref m i)]
                   [y (fx+/32 (unsafe-fxvector-ref m (ind (unsafe-fxrshift x 2))) a b)])
              (unsafe-fxvector-set! m i y)
              (let ([b (fx+/32 (unsafe-fxvector-ref m (ind (unsafe-fxrshift y 10))) x)])
                (unsafe-fxvector-set! r i b)
                (values a b))))])
      (set-randa! ctx a)
      (set-randb! ctx b)
      (set-randc! ctx c)
      ctx)))


(define-syntax mix*
  (syntax-rules ()
    [(_ body) body]
    [(_ (a b c d (sf s)) forms ... body)
     (let* ([a (unsafe-fxxor a (sf b s))]
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


(define (irandom-init/null)
  (scramble! (irand-ctx 0 (make-fxvector 256 0) (make-fxvector 256 0) 0 0 0)))


(define (irandom-init)
  (let ([bs (crypto-random-bytes 1024)])
    (scramble!
     (irand-ctx
      0
      (for/fold ([vec (make-fxvector 256 0)])
                ([i (in-range 0 256)])
        (let ([j (unsafe-fx* i 4)])
          (fxvector-set!
           vec i (fxand #xFFFFFFFF (integer-bytes->integer bs #f #f j (unsafe-fx+ j 4))))
          vec))
      (make-fxvector 256 0)
      0 0 0))))


(define current-irandom-context (make-parameter (irandom-init)))


(define (irandom-32)
  (let* ([ctx (current-irandom-context)]
         [cnt (unsafe-fx- (randcnt ctx) 1)]
         [r (randrsl ctx)])
    (set-randcnt! ctx cnt)
    (cond
      [(<= cnt 0)
       (generate-next-random-block! ctx)
       (set-randcnt! ctx 255)
       (unsafe-fxvector-ref r 255)]
      [else (unsafe-fxvector-ref r cnt)])))

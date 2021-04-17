#lang racket/base

(require (for-syntax racket/base)
         (only-in racket/require filtered-in)
         (only-in racket/unsafe/ops unsafe-string->immutable-string!)
         (filtered-in
          (lambda (name)
            (and (or (regexp-match #rx"^unsafe-fx" name)
                     (regexp-match #rx"^unsafe-bytes" name))
                 (regexp-replace #rx"unsafe-" name "")))
          racket/unsafe/ops)
         "./isaac.rkt")

(provide uuid-bytes uuid-string uuid-string?)


;; The regular expression for checking valid UUIDv4 format is from
;; From https://github.com/LiberalArtist/uuid/tree/master
;;  Copyright © 2018–present Philip McGrath

(define (uuid-string? str)
  (and (or string? bytes?)
       (regexp-match?
        #px"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-4[0-9a-fA-F]{3}-[89ab][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$"
        str)))


(define (uuid-bytes)
  (define-syntax-rule (set-version b) (fxior #x40 (fxand #x0F b)))
  (define-syntax-rule (set-variant b) (fxior #x80 (fxand #x3F b)))
  (define-syntax-rule (->X b) (if (fx< b 10) (fx+ b 48) (fx+ b 87)))
  (define-syntax-rule (copy* dst src sf st df)
    (let loop ([i sf] [j df])
      (when (fx< i st)
        (let ([c (bytes-ref src i)])
          (bytes-set! dst j (->X (fxrshift c 4)))
          (bytes-set! dst (fx+ j 1) (->X (fxand c #xF)))
          (loop (fx+ i 1) (fx+ j 2))))))
  (let ([c- (char->integer #\-)]
        [brnd (irandom-bytes 16)]
        [bout (make-bytes 36)])
    (bytes-set! brnd 6 (set-version (bytes-ref brnd 6)))
    (bytes-set! brnd 8 (set-variant (bytes-ref brnd 8)))
    (copy* bout brnd 0 4 0)
    (bytes-set! bout 8 c-)
    (copy* bout brnd 4 6 9)
    (bytes-set! bout 13 c-)
    (copy* bout brnd 6 8 14)
    (bytes-set! bout 18 c-)
    (copy* bout brnd 8 10 19)
    (bytes-set! bout 23 c-)
    (copy* bout brnd 10 16 24)
    (bytes->immutable-bytes! bout)))


(define (uuid-string)
  (unsafe-string->immutable-string! (bytes->string/latin-1 (uuid-bytes))))

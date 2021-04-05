#lang racket/base

(require racket/fixnum
         racket/unsafe/ops
         "./isaac.rkt")

(provide uuid-bytes uuid-string uuid-string?)


;; The regular expression for checking valid UUIDv4 format is from
;; From https://github.com/LiberalArtist/uuid/tree/master
;;  Copyright © 2018–present Philip McGrath

(define (uuid-string? str)
  (regexp-match?
   #px"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-4[0-9a-fA-F]{3}-[89ab][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$"
   str))


(define (uuid-bytes)
  (define-syntax-rule (set-version b) (fxior #x40 (fxand #x0F b)))
  (define-syntax-rule (set-variant b) (fxior #x80 (fxand #x3F b)))
  (define-syntax-rule (->X b) (if (unsafe-fx< b 10) (unsafe-fx+ b 48) (unsafe-fx+ b 87)))
  (define-syntax-rule (copy* dst src sf st df)
    (let loop ([i sf] [j df])
      (when (fx< i st)
        (let ([c (unsafe-bytes-ref src i)])
          (bytes-set! dst j (->X (unsafe-fxrshift c 4)))
          (bytes-set! dst (unsafe-fx+ j 1) (->X (unsafe-fxand c #xF)))
          (loop (unsafe-fx+ i 1) (unsafe-fx+ j 2))))))
  (let ([c- (char->integer #\-)]
        [brnd (irandom-bytes 16)]
        [bout (make-bytes 36)])
    (unsafe-bytes-set! brnd 6 (set-version (unsafe-bytes-ref brnd 6)))
    (unsafe-bytes-set! brnd 8 (set-variant (unsafe-bytes-ref brnd 8)))
    (copy* bout brnd 0 4 0)
    (unsafe-bytes-set! bout 8 c-)
    (copy* bout brnd 4 6 9)
    (unsafe-bytes-set! bout 13 c-)
    (copy* bout brnd 6 8 14)
    (unsafe-bytes-set! bout 18 c-)
    (copy* bout brnd 8 10 19)
    (unsafe-bytes-set! bout 23 c-)
    (copy* bout brnd 10 16 24)
    bout))


(define (uuid-string) (bytes->string/latin-1 (uuid-bytes)))

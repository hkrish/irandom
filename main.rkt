#lang racket/base

(require racket/contract
         (only-in racket/fixnum fxvector?)
         (only-in racket/flonum flvector?)
         "./isaac.rkt"
         "./uuid.rkt")

(provide irandom-context?
         make-irandom-context
         irandom
         irandom-fixnum
         irandom-32
         (contract-out
          [current-irandom-context (parameter/c irandom-context?)]
          [irandom-bytes (-> fixnum? bytes?)]
          (irandom-fxvector-32 (-> fixnum? fxvector?))
          (irandom-list-32 (-> fixnum? any/c))
          (irandom-fxvector (-> fixnum? fxvector?))
          (irandom-flvector (-> fixnum? flvector?)))
         uuid-string
         uuid-string?)

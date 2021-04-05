#lang racket/base

(require racket/contract
         "./isaac.rkt"
         "./uuid.rkt")

(provide irandom-context?
         make-irandom-context
         irandom
         irandom-fixnum
         irandom-32
         (contract-out
          [current-irandom-context (parameter/c irandom-context?)])
         irandom-bytes
         irandom-fxvector-32
         irandom-list-32
         irandom-fxvector
         irandom-flvector
         irandom-u32vector
         irandom-u64vector
         irandom-f64vector
         uuid-string
         uuid-bytes
         uuid-string?)

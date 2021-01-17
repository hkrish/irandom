#lang racket/base

(require racket/file
         racket/fixnum
         racket/format
         racket/port
         racket/string
         rackunit
         "../issac.rkt")


;; Check output (with a null context) with ./randvect.txt
(let ([bobs-numbers (file->lines "./randvect.txt")]
      [my-numbers
       (string-split
        (with-output-to-string
          (lambda ()
            (parameterize ([current-irandom-context (irandom-init/null)])
              (for ([i (in-range 2)])
                (generate-next-random-block! (current-irandom-context))
                (for ([j (in-range 256)])
                  (display (~r (fxvector-ref (irand-ctx-randrsl (current-irandom-context)) j)
                               #:base 16 #:min-width 8 #:pad-string "0"))
                  (when (= 7 (fxand j 7)) (newline))))))))])
  (check-equal? bobs-numbers my-numbers))

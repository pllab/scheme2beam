;;;; Should not produce an error if implementation supports tail calls.
;;;; Not meant to be an automatic test. Meant to be run manually.

(define (loop n)
  (displayln n)
  (loop (+ 1 n)))

(loop 1)

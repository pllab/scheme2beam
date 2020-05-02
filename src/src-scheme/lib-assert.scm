;;;; Library useful for unit tests.

(define (assert-true v)
  (if v
    (displayln "OK")
    (error "ASSERT-TRUE failed.")))

(define (assert-false v)
  (if v
    (error "ASSERT-FALSE failed.")
    (displayln "OK")))

(define (assert-equal a b)
  (if (equal? a b)
    (displayln "OK")
    (error "ASSERT-EQUAL failed.")))

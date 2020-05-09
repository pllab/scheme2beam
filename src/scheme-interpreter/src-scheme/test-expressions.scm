;;;; Unit tests for Scheme expressions.
(load "lib-assert.scm")

;;; Define, Set!
(define x 1)
(assert-equal x 1)
(set! x 2)
(assert-equal x 2)

;;; Begin.
(define val (begin 1 2 3))
(assert-equal val 3)

;;; Let.
(define x 2)
(let ((x 1)
      (y x)  ; Should be bound to 2, not 1.
      (s "hello"))
  (assert-equal x 1)
  (assert-equal y 2)
  (assert-equal (+ x y) 3)
  (assert-equal s "hello"))

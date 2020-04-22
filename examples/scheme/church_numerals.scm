
;; wikiedia // left-associative
(define z0
  (lambda (f)
    (lambda (x)
      (x))))

(define o1
  (lambda (f)
    (lambda (x)
      (f x))))

(define t2
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define church-add
  (lambda (m)
    (lambda (n)
      (lambda (f)
	(lambda (x)
	  ((m f) ((n f) x)))))))

(define three ((church-add o1) t2))

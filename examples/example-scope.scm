(define (polynomial coefficients x)
	(define (polynomial coefficients value)
		(define (horners-rule free-coefficient)
                  (+ (* value x) free-coefficient)
            )

	(if (null? coefficients)
            value
		(polynomial (cdr coefficients) (horners-rule (car coefficients)))
	)
     )
	(polynomial coefficients 0)
  )


(polynomial `(1 1 1 1) 1)


(define (mult n)
  (lambda (k) (set! n (* k n))
  n)
 )

(define current-n (mult 5))
(current-n 3)
(current-n 4)
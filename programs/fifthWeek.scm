; repeat f n -> returns a new function g = f(f(f(f(....(x)))))
(define (repeat f n)
  (define (helper function counter)
    (if (= counter 0)
        (lambda (x) (function x))
        (helper
         (lambda (x) (function (f x))) (- counter 1))
        )
    )
  (define identity
    (lambda (x)
      x)
    )
  (helper identity n))

(define (derive f)
  (lambda (x)
    (let ((z (+ x 0.00000001)))
      (/ (- (f x)
            (f z))
            (- x z)))))

(define (plusOne x)
  (+ x 1)
  )

(define pow5
  (lambda (x)
    (expt x 5)))

(define (derive-n f n)
    ((repeat derive n) f))

((derive-n pow5 3) 7)
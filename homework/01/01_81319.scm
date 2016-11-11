(define (sum-digits predicate number)
  (define (helper remaining result)
    (let ((lastDigit (remainder remaining 10)) (remainingDigits (quotient remaining 10)))

    (if (= remaining 0)
        result
        (if (predicate lastDigit)
            (helper remainingDigits (+ result lastDigit))
            (helper remainingDigits result)
         )
     )
   ))
  (helper number 0)
)
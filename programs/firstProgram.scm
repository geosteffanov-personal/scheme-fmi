; this is a procedure - an anonymous function, a lambda
; counts the digits of a number
(define (count-digits x)
  (if (= (quotient x 10) 0) 1 (+ 1 (count-digits (quotient x 10) ))))
;(count-digits 10)

;sums the digits of a number
(define (sum-digits number)
  (if (= (quotient number 10) 0) (remainder number 10) (+ (remainder number 10) (sum-digits (quotient number 10)) ) ) )
;(sum-digits 122)


; returns 1 if prime and 0 if not
(define (prime? number)
  (define (helper number start)
    (cond
      ((= start 1) #t)
      ((= (remainder number start) 0) #f)
      (else (helper number (- start 1)))))
  (helper number (- number 1)))

;(prime? 6)

; returns #t if a number is automorphic and #f otherwise
(define (automorphic? number)
  (= number (remainder (expt number 2)  (expt 10 (count-digits number)))))
;(automorphic? 76)

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



(define (my-reverse n)
  (if (< n 10)
       n
       (+ (* (remainder n 10) (expt 10 (- (count-digits n) 1) )) (my-reverse (quotient n 10)))))

(define (palindrome? n)
  (define (extract-middle n)
    (quotient (remainder n (expt 10 (- (count-digits n) 1))) 10))
   (if (< n 10) #t
       ; n >= 10
       (and ( = (quotient n (expt 10 (- (count-digits n) 1))) (remainder n 10)) (palindrome? (extract-middle n) ))))

(define (palindrome2? n)
  (define (helper current-n result)
    (cond
      ((> result current-n) #f)
      ((= result current-n) #t)
      ((= result (quotient current-n 10)) #t)
      (else (helper (quotient current-n 10) (+ (* result 10) (remainder current-n 10))))))
  (helper n 0)
 )
                 

(define (count-palindromes start end)
  (define (helper current number)
    (if (> current end)
         number
        (helper (+ current 1) (if (palindrome? current) (+ number 1) number))
     )
   )
  (helper start 0)
 )

(define (substr n a)
  (if (not (> n a))
      (= n a)
      (or (= a (remainder n (expt 10 (count-digits a)))) (substr (quotient n 10) a))))

(define (fibonacci-iter n)
  (define (helper counter prevA prevB)
     (if (= counter (- n 1))
         prevB
         (helper (+ 1 counter) prevB (+ prevA prevB))))
  (helper 0 0 1)
 )

(define (fibonacci-rec n)
  (if (or (= n 1) (= n 2)) 1 (+ (fibonacci-rec (- n 1)) (fibonacci-rec (- n 2)))))

(fibonacci-iter 200000)
         
    

     
  

  
  
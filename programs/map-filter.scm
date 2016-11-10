(load "../fpkn1617/lib/scm/unit.scm")

(map (lambda (x) ( * x x)) `(1 2 3))

#! well this does what it does !#
(define (filter predicate list)
  (if (null? list)
      `()
      (if (predicate (car list))
          (cons (car list) (filter predicate (cdr list)))
          (filter predicate (cdr list))
      )
  )
)
#! better filter !#
(define (FILTER p l)
  (define (helper current-l result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)
                                   (append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))

(filter even? `(1 2 3 4 5))
(assert-equal `(1 2 3 4 5) (filter (lambda (x) (< x 6)) `( 1 2 3 4 5 6 7 8 9 10 11)))
(assert-equal `() (filter (lambda (x) (> x 6)) `(1 2 3 4 5)))

#! tests wheter an element is a member of a list #!
(define (member? element list)
  (if (null? list)
      #f
      (or (= (car list) element) (member? element (cdr list)))))

#! union l1 l2 !#
(define (UNION A B)
  (if (null? A)
      B
     (if (member? (car A) B)
         (UNION (cdr A) B)
         (cons (car A) (UNION (cdr A) B)))))
(UNION `(1 2 3 4 5 ) `(2 3 4 5 6))

#! intersection l1 l2 !#
(define (INTERSECTION A B)
  (filter (lambda (x) (member? x B)) A)
)

(define (DIFFERENCE A B)
  (filter (lambda (x) (not (member? x B))) A))

(INTERSECTION `(1 2 3 4) `(2 3 4 5))
(DIFFERENCE `(1 2 3 4 5 6 7) `(3 4 5))

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


(define (COMPOSITION-LIST list function)
  (if (null? list)
      `()
      (cons (repeat function (car list)) (COMPOSITION-LIST (cdr list) function))))


#! returns the nth element of a list !#
(define (nth-elem l1st index)
  (define (helper crrIndex currentEl remList)
    (if (= index crrIndex)
        currentEl
        (if (null? remList)
            `()
            (helper (+ index 1) (car (cdr remList)) (cdr remList)))
    ))
  (if (null? l1st)
      `()
      (helper 0 (car l1st) l1st))
  )

(multi-map list '(1 2 3) '(4 5 6) '(7 8 9))
(TRANSPOSE `((1 2 3) (4 5 6) (7 8 9)))


  

 
  
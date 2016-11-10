(load "../fpkn1617/lib/scm/unit.scm")

#! tests wheter an element is a member of a list #!
(define (member? element list)
  (if (null? list)
      #f
      (or (= (car list) element) (member? element (cdr list)))))

#! returns the lengths of a list !#
(define (my-length list)
  (if (null? list)
      0
      (+ 1 (my-length (cdr list)))))

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




#! appends the l2 at the back of l1 !#
(define (my-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (my-append (cdr l1) l2))))

#! constructs a list of all numbers from 1 to 10^n !#
(define (construct-list powOfTen)
  (define (helper crrIndex crrList)
       (if (= crrIndex 0)
           crrList
           (helper (- crrIndex 1) (cons crrIndex crrList))
       )
   )
  (helper (expt 10 powOfTen) `())
)

#! reverses a list !#
(define (my-reverse list)
  (define (helper remainingList currentList)
    (if (null? remainingList)
        currentList
        (helper (cdr remainingList) (cons (car remainingList) currentList))))

  (helper list `())
  )

#! checks if x is a member of l, where l may be a list of lists !#
(define (my-member x l)
  (if (null? l)
      #f
      (or (and (not (list? (car l))) (= x (car l)))
          
          (and (list? (car l)) (my-member x (car l)))
          
          (my-member x (cdr l)))))


#! flattens a list of lists into asingle lists) !#
(define (flatten l)
  (if (null? l)
      `()
      (if (list? l)
          (my-append (flatten (car l)) (flatten (cdr l)))
          (list l)
       )

      )
  )

(assert= 0 (my-length `()))
(assert= 1 (my-length `(2)))
(assert= 3 (my-length `(3, 1, 1)))

(assert-true (member? 2 `(1 4 2 3)))
(assert-false (member? 5 `(1 4 2 3)))

(assert-equal `(1 2 3 4) (my-append `(1) `(2 3 4)))
(assert-equal `(1 2 3 4) (my-append `(1 2) `(3 4)))
(assert-equal `(1 2 3 4) (my-append `(1 2 3) `(4)))
(assert-equal `(1 2 3 4) (my-append `(1 2 3 4) `()))

(assert-eq `() (nth-elem `() 5))
(assert-eq 1 (nth-elem `(1) 0))


  
  


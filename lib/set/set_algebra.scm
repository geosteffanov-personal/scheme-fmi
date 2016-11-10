#! FUNCTIONS USED FOR ALGEBRA OF SETS !#


(define (filter p l)
  (define (helper current-l result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)
                                   (append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))


#! (SET list) => (a set containing no duplicates)
(define (SET list)
  (define (helper result remainingList)
    (if (null? remainingList)
        result
        (if (member (car remainingList) result)
            (helper result (cdr remainingList))
            (helper (cons (car remainingList) result) (cdr remainingList))
        )
    )
   )
  (reverse (helper `() list))
 )

#! (UNION A B) => (the union of the sets A and B) !#
(define (UNION A B)
  (SET (append A B))
)

#! (INTERSECTION A B) !#
(define (INTERSECTION A B)
  (SET (filter (lambda (x) (member x B)) A))
)

#! (DIFFERENCE A B) !#
(define (DIFFERENCE A B)
  (SET (filter (lambda (x) (not (member x B))) A) )
)

#! (DISJUNCTIVE-UNION A B) !#
(define (DISJUNCTIVE-UNION A B)
  (UNION (DIFFERENCE A B) (DIFFERENCE B A))
)

(define (CARTESIAN-PRODUCT A B)
  (if (or (null? A) (null? B))
      `()
      (append (map (lambda (x) (cons (car A) x)) B) (CARTESIAN-PRODUCT (cdr A) B))
  )
 )
#! SINGLETON A !#
(define (SINGLETON A)
  (if (not (list? A))
      (list A)
      (list (SET A)))
)

#! POWER-SET A !#
(define (POWER-SET A)
  (if (null? A)
      `(())
      (UNION (POWER-SET (cdr A))
             (map (lambda (x) (UNION (SINGLETON (car A)) x)) (POWER-SET (cdr A)))
       )

  )
)

#! CARDINALITY A !#
(define (CARDINALITY A)
  (if (null? A)
      0
      (+ 1 (CARDINALITY (cdr A)))
   )
 )








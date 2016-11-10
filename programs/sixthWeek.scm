(load "../fpkn1617/lib/scm/unit.scm")

(define (my-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (my-append (cdr l1) l2))))

(define (my-reverse list)
  (define (helper remainingList currentList)
    (if (null? remainingList)
        currentList
        (helper (cdr remainingList) (cons (car remainingList) currentList))))

  (helper list `())
  )

(define (my-member x l)
  (if (null? l)
      #f
      (or (and (not (list? (car l))) (= x (car l)))
          
          (and (list? (car l)) (my-member x (car l)))
          
          (my-member x (cdr l)))))



(define (flatten l)
  (if (null? l)
      `()
      (if (list? l)
          (my-append (flatten (car l)) (flatten (cdr l)))
          (list l)
       )

      )
  )

(flatten `(1 2 (3 4 ) 5 (6 (9 1 (13))7)))
 
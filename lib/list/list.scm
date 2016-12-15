#! tests whether an element is a member !#
(define (IS-MEMBER? element list)
  (if (null? list)
      #f
      (or (= (car list) element) (IS-MEMBER? element (cdr list)))
      )
  )

#! returns the length of a list !#
(define (LIST-LENGTH list)
  (if (null? list)
      0
      (+ 1 (LIST-LENGTH(cdr list)))
      )
  )


#! returns the n-th element of a list !#
(define (AT-INDEX index list)
  (define (FOR currentIndex currentList)
    (if (= currentIndex index)
        (car currentList)
        (FOR (+ currentIndex 1) (cdr currentList))
     )
   )
  (FOR 0 list)
 )

#! concatenates two lists !#
(define (CONCATENATE list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (CONCATENATE (cdr list1) list2))
   )
)


#! reverses a list !#
(define (LIST-REVERSE list)
  (define (WHILE currentList remainingList)
    (if (null? remainingList)
        currentList
        (WHILE (cons (car remainingList) currentList) (cdr remainingList))
    )
 )
 (WHILE `() list))


#! checks if a list contains an element x, or it contains a list containing x ( or ... ) !#
(define (UNWIND-MEMBER? x _list)
  (if (null? _list)
      #f
      (or (and (not (list? (car _list))) (= (car _list) x))
          (and (list? (car _list)) (UNWIND-MEMBER? x (car _list)))
          (UNWIND-MEMBER? x (cdr _list))
       )
  )
 )

#! flattens a list of lists into a single list !#
(define (FLATTEN l)
  (if (null? l)
      `()
      (if (list? l)
          (CONCATENATE (FLATTEN (car l)) (FLATTEN (cdr l)))
          (list l)
       )
  )
)


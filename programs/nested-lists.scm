#! apply  !#
(apply + `(1 2 3))
(+ 1 2 3)

#! doesnt work - (apply + `(1 2) 3), neither does (apply + 1 2 3) !#

(apply + (map (lambda (row) (apply * row) ) `((1 2) (3 4))))

#! fold <=> inject <=> reduce  !#

(define (COMPOSE-LIST list)
  (fold-left (lambda (result x) (lambda (y) (result (x y)))) (lambda (x) x) list)
 )

(define (MULT2 x)
  (* x x)
 )

((COMPOSE-LIST (list MULT2 MULT2)) 2)

(define (SUM-LISTS l x)
	(fold-left 
	           (lambda (result y) 
	           		(if (member x y)
	                    (+ (fold-left + 0 y) result)
	                    result
	                )
	            )
	           0
	           l
	)
)

(SUM-LISTS `((1 2) (1 4) (5 6)) 1)


(define (NTH list index)
  (define (helper current remaining)
    (if (= current index)
        (car remaining)
        (helper (+ current 1) (cdr remaining))
     )
  )
  (helper 0 list)
 )

(define (SIZE list)
	(if (null? list)
	    0
	    (+ 1 (SIZE (cdr list)))
	)
)

(define (MAIN-DIAGONAL list)
  (define (helper currentIndex result)
    (if (= currentIndex (SIZE list))
        result
        (helper (+ 1 currentIndex) (append result (list (NTH (NTH list currentIndex) currentIndex)))))
  )
  (helper 0 `())
)


(define (NTH list index)
  (define (helper current remaining)
    (if (= current index)
        (car remaining)
        (helper (+ current 1) (cdr remaining))
     )
  )
  (helper 0 list)
 )

(define (SIZE list)
	(if (null? list)
	    0
	    (+ 1 (SIZE (cdr list)))
	)
)


(define (MAIN-DIAGONAL lest)
  (define (helper currentIndex result)
    (if (= currentIndex (SIZE lest))
        result
        (helper (+ 1 currentIndex) (append result (list (NTH  (NTH lest currentIndex) currentIndex)))))
  )
  (helper 0 `())
)

(define (ITH-COLUMN matrix column)
	(define (helper currentIndex result)
		(if (= currentIndex (SIZE matrix))
		       result
		       (helper (+ 1 currentIndex) (append result (list (NTH (NTH matrix currentIndex) column))))
		)
	)
	(helper 0 `())
)



(MAIN-DIAGONAL `((1 2 3) (4 5 6) (7 8 9)))
(ITH-COLUMN `((1 2 3) (4 5 6) (7 8 9)) 1)

(define (FILTER-MATRIX p m)
	(map (lambda (x) (filter p x)) m)
)
(FILTER-MATRIX even? `((1 2 3) (4 5 6) (7 8 9)))

(define (SKIP-ROW matrix row)
	(define (helper currentIndex result remaining)
		(if (null? remaining)
		    result
			(if (= currentIndex row)
		    (helper (+ 1 currentIndex) result (cdr remaining))
		    (helper (+ 1 currentIndex) (append result (list (car remaining))) (cdr remaining))))
	)
(helper 0 `() matrix))

(SKIP-ROW `((1 2 3) (4 5 6) (7 8 9)) 0)


(define (TRANSPOSE m)
	(define (helper index result)
		(if (= index (SIZE m))
		    result
			(helper (+ 1 index)	(append result  (list (map (lambda (x) (NTH x index)) m)))
			)
		)
	)
(helper 0 `()))
(TRANSPOSE `((1 2 3) (4 5 6) (7 8 9)))

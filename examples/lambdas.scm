; (define sum-nums
; 	(case-lambda
; 		([x] (fold-left + 0 x))
; 		([x . lis] (fold-left + 0 (cons x lis)))
; 	)
; )

; (define merge
; 	(case-lambda
; 		([a b]
; 			(cond
; 				([null? a] b)
; 				([null? b] a)
; 			(else (let ([x (car a)] [y (car b)])
; 						(if (<= x y)
; 							(cons x (merge (cdr a) b))
; 							(cons y (merge a (cdr b))))
; 				   )
; 			)
; 			)
; 		)
; 		(a (fold-left merge '() a))
; ))

; (merge `(2 8 2 73 2 3) `(1 23 5 2))
(define (TRANSPOSE m)
  (apply map list m))

(define (merge-lists l1 l2)
  (if (null? l1) 
      l2
      (if (null? l2)
          l1
          (let ((crrL (car l1)) (crrR (car l2)))
               (if (<= crrL crrR)
                   (cons crrL (merge-lists (cdr l1) l2))
                   (cons crrR  (merge-lists l1 (cdr l2))))))))
(define list-head
   (lambda (lst k)
      (if (= k 0)
         '()
          (cons (car lst)(list-head (cdr lst)(- k 1))))))

(define list-tail 
   (lambda (lst k)
     (if (= k 0)
         lst
         (list-tail (cdr lst) (- k 1)))))

(define (merge-sort l1)
  (if (= (length l1) 1)
      l1
      (let* ( (halfLen (div (length l1) 2))
              (first (list-head l1 halfLen))
              (second (list-tail l1 halfLen)))
        (merge-lists (merge-sort first) (merge-sort second)))))

; (define my-min
;   (case-lambda
;     ([x] x)
;     ([x y] 
;       (if (< x y) x y))
;     (x (min x))))
; (my-min 1)

; (define (sum lst)
;   (do ((sum 0 (+ (car lest) sum))(lest lst (cdr lest))) (not (null? lest)) sum))
; (sum `(1 2 3 4 5 6))

; (define (extrema l1)
;   (define (helper result current)
;       (if (= (length current) 2)
;           result
;           (let* ((curr (cadr current))
;                 (prev (car current))
;                 (next (caddr current))
;                 (incoming (- curr prev))
;                 (outgoing (- curr next))
;                 (diff (* incoming outgoing)))

;               (if (< diff 0)
;                   (helper result (cdr current))
;                   (helper (append result (list curr)) (cdr current))))))
    
;   (helper `() l1))

; (extrema `(1 2 3 4 5 6 7))

; (define (intertwine f g)
;   (lambda (x)
    
;     (define (applyF  result remaining)
;       (if (null? remaining)
;           result
;           (applyG (append result (map f (car remaining)) ) (cdr remaining))
;       )
;     )
;     (define (applyG  result remaining)
;       (if (null? remaining)
;           result
;           (applyF (append result (map g (car remaining)) ) (cdr remaining))
;       )
;     )
;     (applyF `() x)
;   )
; )
; ((intertwine (lambda (x) (* x 0)) (lambda (y) (* y -2))) `((1 2 3) (4 5 6) (7 8 9)))

; (define (unlimited-stream n)
;   (define crr n)
;   (define increasing #t)
;   (lambda ()
;     (if (= crr n)
;         (begin
;           (let ((result crr))
;           (set! increasing #f)
;           (set! crr (- crr 1))
;             result
;           )
;         )
;         (if (= crr 1)
;             (begin
;               (let ((result crr))
;               (set! increasing #t)
;               (set! crr (+ crr 1))
;               result
;               )
;             )
;             (begin 
;               (if increasing
;                   (begin (let ((result crr))(set! crr (+ crr 1)) result))
;                   (begin (let ((result crr))(set! crr (- crr 1)) result))
;               )
;             )
;         )
;     )
;   )
; )

; (define stream5 (unlimited-stream 5))
; (stream5)
; (stream5)
; (stream5)
; (stream5)
; (stream5)
; (stream5)
; (stream5)

(define graph1 
  `(
    (1 (2 3 4))
    (2 (3 4 5))
    (3 ())
    (4 ())
    (5 ())
  )
)

(define graph2 
  `(
    (1 (2 4))
    (2 (4 5))
    ; (3 ())
    (4 ())
    (5 ())
  )
)


(define (graph-intersection firstG secondG)
  (filter 
          (lambda (x) (member (car x) (map (lambda (x) (car x)) secondG))) 
          (map 
              (lambda (row)
                (let ((vertex (car row)))  
                  (cons vertex 
                   
                    (list 
                      ()
                    )     
                  
                  )
                )
              )
            firstG
          )
  )
)

(graph-intersection graph1 graph2)


              
        
                 

               
              
`(1 
    (2 
       () 
       ()) 
    (3 (4 
          () 
          ()) 
       ()))

(define (ROOT-VAL tree)
  (car tree)
)

(define (LEFT-SUBTREE tree)
  (cadr tree)
)

(define (RIGHT-SUBTREE tree)
  (caddr tree)
)

(define (EMPTY-TREE) `())
(define (EMPTY? tree) (null? tree))
(define (CONS-TREE root left right)
  (list root left right)
)
(define (LEAF? tree)
  (and (not (EMPTY? tree))
       (EMPTY? (LEFT-SUBTREE tree))
       (EMPTY? (RIGHT-SUBTREE tree))
  ))

(define (PRE-ORDER tree)
  (if (LEAF? tree)
      (ROOT-VAL tree)
      (append (list (ROOT-VAL tree)) (list(PRE-ORDER (LEFT-SUBTREE tree)))
                              (list (PRE-ORDER (RIGHT-SUBTREE tree))))
  ))

(define (LEVEL tree level)
  (define (helper currLevel currTree)
    (if 😊 currLevel level)
        (ROOT-VAL currTree)
        (append (list (helper (+ currLevel 1) (LEFT-SUBTREE currTree)))
                (list (helper (+ currLevel  1) (RIGHT-SUBTREE currTree)))))
    )
   (helper 0 tree)
)
(define (MAP-TREE function tree)
  (if (EMPTY? tree)
      (EMPTY-TREE)
      (CONS-TREE (function (ROOT-VAL tree))
                (MAP-TREE function (LEFT-SUBTREE tree))
                (MAP-TREE function (RIGHT-SUBTREE tree)))))


                  
(define (CONTAINS-PATH? tree p)
  (if (null? tree)
      #f
      (if (LEAF? tree)
          😊 (ROOT-VAL tree) p)
          (or (CONTAINS-PATH? (LEFT-SUBTREE tree) p)
              (CONTAINS-PATH? (RIGHT-SUBTREE tree) p)))))
            
(define (LIST-PATHS tree)
  (define helper result)
    (if (null? tree)
        `()
      (append (list (ROOT-VAL tree)) (LIST-PATHS (LEFT-SUBTREE tree)))
      (append (list (ROOT-VAL tree)) (LIST-PATHS (RIGHT-SUBTREE tree)))
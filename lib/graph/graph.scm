(define graph
  (case-lambda
    ( () `(*progenitor))
    ( (x) (map (lambda (y) (list y `())) x))
  )
)

(define (add-edge vertex child graph)
  (let ((crr (assoc vertex graph))
      (if (crr)
        (let  ( (children (cdr crr)) )
          (if (member child children)
            child
            (begin (cons child children) child)
          )
        )
  )
)

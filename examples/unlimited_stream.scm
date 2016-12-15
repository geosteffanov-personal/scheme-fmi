(define (unlimited-stream n)
  ( let((crr n)
  	(increasing #t))
  (lambda ()
    (if (= crr n)
        (begin
          (let ((result crr))
          (set! increasing #f)
          (set! crr (- crr 1))
            result
          )
        )
        (if (= crr 1)
            (begin
              (let ((result crr))
              (set! increasing #t)
              (set! crr (+ crr 1))
              result
              )
            )
            (begin 
              (if increasing
                  (begin (let ((result crr))(set! crr (+ crr 1)) result))
                  (begin (let ((result crr))(set! crr (- crr 1)) result))
              )
            )
        )
    )
  ))
)

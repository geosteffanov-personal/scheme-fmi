(define (unlimited-stream+))
	(let* ((crr 0))
		(begin
		   (let ((prev crr))
		   	 (begin
		   	 	(set! crr (+ crr 1))
		   	 	prev
		   	 )
		   	)
		)
	)
)

; (do ((x 0 (+ x 1)))
;     ((= x 10) x)
;     (begin 
;       (display x)
;       (display x)
;       (newline)
;     )
; )


; (define (invert-args f)
;   (lambda (x y)
;     (f y x)
;   )
; )

; (- 3 4)
; ((invert-args -) 3 4)

; (define (my-sum x y)
;   (eval `(+ ,x ,y))
; )

; (my-sum 3 4)

; (quasiquote x)
; `(1 ,(+ 2 3) 4)
; (define x 5)
; '(,@x)
; (define my-even even?)
; (define qq quasiquote)
; (define q quote)
; (define uq unquote)
; (define x 5)
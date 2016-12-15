#!r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))
;(display (find even? '(3 1 4 1 5 9)))
;(display (fold-right + 0 `(1 2 3 4 5 6)))

;(display (let ((x 5)) x))(newline)
;(display (let ((x 5) (y (+ x 3)))  y)) <---- WILL NOT RUN x is not bound!!
;(display (let* ((x 5) (y (+ x 3)))  y))(newline)
(do ((x 1 (+ x 1))) ((< x 10) (display x)))



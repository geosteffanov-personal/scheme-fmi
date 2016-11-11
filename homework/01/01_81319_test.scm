(load "01_81319.scm")
(load "../../lib/assert/unit.scm")

(assert= 12 (sum-digits even? 123456))
(assert= 9 (sum-digits odd? 123456))
(assert= 21 (sum-digits (lambda (x) #t) 123456))
(assert= 0 (sum-digits (lambda (x) #f) 123456))
(assert= 11 (sum-digits (lambda (x) (or (= x 3) (= x 4))) 11324984))
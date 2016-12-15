#lang racket
; LAZY potoci i spisaci
; bezkraini potoci i spisaci
; map, filter i t.n. varhu bezkraini potoci

;(delay <
;   spisaci                streams
;    `()                     empty-stream
;    cons                   stream-cons
;   null?                   stream-empty?
;   length                  stream-length
;   list-ref                stream-ref
;   append                  stream-append
;   car                     stream-first
;   cdr                     stream-rest
;   list                    stream
;   map                     streasm-map
;  filter                   stream-filter

; stream->list

;(enum start end)
;(list-to-stream l)
;(stream-to-list s)
; repeat-list l
; repeat-lists l1 l1
; cartesian-product A1 A2


(define (enum start end)
  (define (helper stream crr)
    (if (= (- start 1) crr)
        stream
        (helper (stream-cons crr stream) (- crr 1))
    )
   )
  (helper empty-stream end)
 )

(define (stream-to-list strim)
  (define (helper crrstrm crrlst)
    (if (stream-empty? crrstrm)
        crrlst
        (helper (stream-rest crrstrm) (append crrlst (list (stream-first crrstrm))))
     )
   )
  (helper strim `())
)

; list-to-stream -DOMASHNO
(define (stream-take s n)
  (define (helper crrtaken crrlst crrstrm)
    (if (= crrtaken n)
        crrlst
        (helper (+ crrtaken 1) (append crrlst (list (stream-first crrstrm))) (stream-rest crrstrm))
      )
   )
  (helper 0 `() s)
 )

(define (stream-take-rec s n)
  (if (= n 0)
      `()
      (cons (stream-first s) (stream-take-rec (stream-rest s) (- n 1)))))

(define (add-streams s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      empty-stream
      (stream-cons (+ (stream-first s1) (stream-first s2)) (add-streams (stream-rest s1) (stream-rest s2)))
    )
 )

(define (n-stream)
  (define (helper n)
    (stream-cons n (helper (+ n 1)))
   )
  (helper 0)
 )

(define (negative-n-stream)
  (map (lambda (x) (* x -1)))
 )

(define (isPrime? x)
  (define (isNotDivisible? r)
    (define (helper crr)
      (if (= crr 1)
          #t
          (and (not (= (remainder r crr) 0)) (helper (- crr 1))))
     )
    (helper (- r 1))
  )
  (if (or (= 0 x) (= 1 x))
      #f
      (isNotDivisible? x)
   )
 )
    
(define (prime-stream)
  (stream-filter (lambda (x) (isPrime? x)) (n-stream)))


(define (fibonacci-stream)
  (define (helper crrPrev crrPrevPrev)
    (stream-cons (+ crrPrev crrPrevPrev) (helper (+ crrPrev crrPrevPrev) crrPrev))
   )
  (stream-cons 1 (stream-cons 1 (helper 1 1)))
 )

(define (stream-cartesian A B)
  (define (helper crrA crrB crrLeftA crrLeftB AorB iteratedIndex maxIndex)
    (cond
      [(and (= iteratedIndex maxIndex) (= 1 AorB))

        (stream-cons (cons (stream-first crrLeftA) (stream-first crrLeftB))
                     (helper A B (stream-rest crrLeftA)(stream-rest crrLeftB) 0 0 (+ 1 maxIndex)))]
        
      [(and (= iteratedIndex maxIndex) (= 0 AorB))
        (helper A B crrLeftA crrLeftB 1 0 maxIndex)]

      [(= 1 AorB)
       
       (stream-cons (cons (stream-first crrA) (stream-first crrLeftB))
                    (helper (stream-rest crrA) B crrLeftA crrLeftB 1 (+ iteratedIndex 1) maxIndex))]

      [(= 0 AorB)
       (stream-cons (cons (stream-first crrLeftA) (stream-first crrB))
                    (helper A (stream-rest crrB) crrLeftA crrLeftB 0 (+ iteratedIndex 1) maxIndex))]
      

    )
  )
  (helper A B A B 0 0 0)
)




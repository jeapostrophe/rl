#lang scheme
(define (randomize-vector v)
  (for ([n (in-range (vector-length v) 0 -1)])
    (define r (random n))
    (define t (vector-ref v r))
    (vector-set! v r (vector-ref v (- n 1)))
    (vector-set! v (- n 1) t)))

; permute-list: (listof a) -> (listof a)
; returns a new list with the elements of the old in a random order
(define (permute-list lst)
  (define v (list->vector lst))
  (randomize-vector v)
  (vector->list v))

(define (random-element l)
  (list-ref l (random (length l))))

(define (random-in lo hi)
  (+ lo (random (add1 (- hi lo)))))

(provide/contract
 [random-in (->d ([lo exact-positive-integer?]
                  [hi exact-positive-integer?])
                 ()
                 #:pre-cond (<= lo hi)
                 [rnd exact-positive-integer?])]
 [random-element ((listof any/c) . -> . any/c)]
 [permute-list ((listof any/c) . -> . (listof any/c))])
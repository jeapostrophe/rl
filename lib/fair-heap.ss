#lang scheme
; The pairing heap doesn't work well because it doesn't guarantee newer smaller things run after
(require "batched-deque.ss")

(require (planet wmfarr/pairing-heap/pairing-heap)
         srfi/67)

(define-values/invoke-unit pairing-heap@
  (import (rename compare^ (integer-compare compare)))
  (export (prefix heap: pairing-heap^)))

(define-struct heap (energy-q energy->list-map))

(define fair-heap-empty (make-heap (heap:heap) (make-immutable-hasheq empty)))
(define (fair-heap-insert energy obj hp)
  (define h (heap-energy->list-map hp))
  (define eq (heap-energy-q hp))
  (define new-eq (heap:insert energy eq))
  (define new-h
    (hash-update 
     h energy
     (lambda (old)
       (deque-push obj old))
     deque-empty))
  #;(printf "insert ~a ~a~n" energy obj)
  (make-heap new-eq new-h))

(define (fair-heap-min hp)
  (define h (heap-energy->list-map hp))
  (define eq (heap-energy-q hp))
  (define min-e (heap:min eq))
  (define min-q (hash-ref h min-e))
  (deque-first min-q))

(define (fair-heap-remove-min hp)
  (define h (heap-energy->list-map hp))
  (define eq (heap-energy-q hp))
  (define min-e (heap:min eq))
  (define min-q (hash-ref h min-e))
  (define next-eq (heap:remove-min eq))
  (define next-q (deque-shift min-q))
  #;(printf "remove ~a~n" min-e)
  (if (deque-empty? next-q)
      (make-heap next-eq (hash-remove h min-e))
      (make-heap next-eq (hash-set h min-e next-q))))

(define fair-heap? heap?)
(define (fair-heap-elements hp)
  (apply append
         (hash-map (heap-energy->list-map hp)
                   (lambda (k v)
                     (deque-elements v)))))

(define (fair-heap-fold f a h)
  (foldl f a (fair-heap-elements h)))

(define (fair-heap-map f h)
  (fair-heap-fold 
   (lambda (e a)
     (define-values (en o) (f e))
     (fair-heap-insert en o a))
   fair-heap-empty
   h))

(provide/contract
 [fair-heap-empty fair-heap?]
 [fair-heap-insert (exact-integer? any/c fair-heap? . -> . fair-heap?)]
 [fair-heap-min (fair-heap? . -> . any/c)]
 [fair-heap-remove-min (fair-heap? . -> . fair-heap?)]
 [fair-heap? (any/c . -> . boolean?)]
 [fair-heap-elements (fair-heap? . -> . (listof any/c))]
 [fair-heap-fold 
  ((any/c any/c . -> . any/c)
   any/c
   fair-heap? . -> . any/c)]
 [fair-heap-map
  ((any/c . -> . (values exact-integer? any/c))
   fair-heap?
   . -> . 
   fair-heap?)])
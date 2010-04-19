#lang scheme
(require "../lib/matrix.ss"
         #;(planet jaymccarthy/matrix)
         "posn.ss")

(define (matrix-set/posn m p v)
  (matrix-set m (posn-row p) (posn-col p) v))
(define (matrix-ref/posn m p)
  (matrix-ref m (posn-row p) (posn-col p)))
(define (matrix-update/posn m p f)
  (matrix-update m (posn-row p) (posn-col p) f))
  
(define (matrix-valid-ref?/posn m p)
  ((make-valid-posn? (matrix-rows m) (matrix-cols m)) p))

#;(provide (except-out
          (all-from-out (planet jaymccarthy/matrix))
          display-matrix))
(provide (all-from-out "../lib/matrix.ss"))
(provide/contract
 [matrix-set/posn (matrix? posn? any/c . -> . matrix?)]
 [matrix-update/posn (matrix? posn? (any/c . -> . any/c) . -> . matrix?)]
 [matrix-ref/posn (matrix? posn? . -> . any/c)]
 [matrix-valid-ref?/posn (matrix? posn? . -> . boolean?)])
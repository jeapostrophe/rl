#lang scheme

(define-struct matrix (rows cols ht))

(define (matrix-index m ri ci)
  (+ ci (* (matrix-cols m) ri)))
  
(define (build-matrix rows cols f)
  (define m (make-matrix rows cols (make-immutable-hasheq empty)))
  (for*/fold ([m m])
    ([ri (in-range 0 rows)]
     [ci (in-range 0 cols)])
    (matrix-set m ri ci (f ri ci))))

(define (matrix-valid-ref? m r c)
  (cond
    [(r . >= . (matrix-rows m))
     #f]
    [(c . >= . (matrix-cols m))
     #f]
    [else
     #t]))

(define (matrix-ref m r c)
  (if (matrix-valid-ref? m r c)
      (hash-ref (matrix-ht m) (matrix-index m r c))
      (error 'matrix-ref "Invalid reference")))

(define (matrix-update m r c f)
  (if (matrix-valid-ref? m r c)
      (struct-copy matrix m
                   [ht (hash-update (matrix-ht m) (matrix-index m r c) f)])
      (error 'matrix-sef "Invalid reference")))

(define (matrix-set m r c v)
  (if (matrix-valid-ref? m r c)
      (struct-copy matrix m
                   [ht (hash-set (matrix-ht m) (matrix-index m r c) v)])
      (error 'matrix-sef "Invalid reference")))            

(provide/contract
 [matrix-rows (matrix? . -> . exact-positive-integer?)]
 [matrix-cols (matrix? . -> . exact-positive-integer?)]
 [matrix? (any/c . -> . boolean?)]
 [build-matrix
  (exact-positive-integer?
   exact-positive-integer?
   (exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c)
   . -> .
   matrix?)]
 [matrix-valid-ref? (matrix? exact-nonnegative-integer? exact-nonnegative-integer? . -> . boolean?)]
 [matrix-ref (matrix? exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c)]
 [matrix-set (matrix? exact-nonnegative-integer? exact-nonnegative-integer? any/c . -> . matrix?)]
 [matrix-update (matrix? exact-nonnegative-integer? exact-nonnegative-integer?  (any/c . -> . any/c) . -> . matrix?)])
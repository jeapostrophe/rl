#lang scheme
(require tests/eli-tester)

(define-struct posn (row col) #:transparent)

; XXX weak values
(define make-posn*
  (local [(define posns (make-hasheq))]
    (lambda (r c)
      (hash-ref! (hash-ref! posns r (lambda () (make-hasheq)))
                 c (lambda () (make-posn r c))))))

(define ((make-valid-posn? max-row max-col) p)
  (not
   (or ((posn-row p) . < . 0)
       ((posn-col p) . < . 0)
       ((posn-row p) . >= . max-row)
       ((posn-col p) . >= . max-col))))

(define (posn=? p r c)
  (and (= r (posn-row p))
       (= c (posn-col p))))

(define (posn<= p1 p2)
  (cond
    [(< (posn-row p1) (posn-row p2))
     #t]
    [(= (posn-row p1) (posn-row p2))
     (<= (posn-col p1) (posn-col p2))]
    [else
     #f]))

(test
 (posn<= (make-posn 0 0) (make-posn 0 0)) => #t
 (posn<= (make-posn 0 0) (make-posn 1 0)) => #t
 (posn<= (make-posn 0 0) (make-posn 0 1)) => #t
 (posn<= (make-posn 0 0) (make-posn 1 1)) => #t
 (posn<= (make-posn 2 1) (make-posn 1 1)) => #f
 (posn<= (make-posn 11 62) (make-posn 11 61)) => #f)

(define (posn-adjust p dr dc)
  (define nrow (+ dr (posn-row p)))
  (define ncol (+ dc (posn-col p)))
  (make-posn* nrow ncol))

(define (posn-left p) (posn-adjust p 0 -1))
(define (posn-right p) (posn-adjust p 0 1))
(define (posn-up p) (posn-adjust p -1 0))
(define (posn-down p) (posn-adjust p 1 0))

(define (posn-distance p1 p2)
  (sqrt (+ (sqr (- (posn-row p1) (posn-row p2)))
           (sqr (- (posn-col p1) (posn-col p2))))))

(define (posn-direction p1 p2)
  (cond
    [(= (posn-row p1) (posn-row p2))
     'horiz]
    [(= (posn-col p1) (posn-col p2))
     'vert]
    [(and (< (posn-row p1) (posn-row p2))
          (< (posn-col p1) (posn-col p2)))
     'dright]
    [(and (> (posn-row p1) (posn-row p2))
          (> (posn-col p1) (posn-col p2)))
     'uleft]
    [(and (> (posn-row p1) (posn-row p2))
          (< (posn-col p1) (posn-col p2)))
     'uright]
    [(and (< (posn-row p1) (posn-row p2))
          (> (posn-col p1) (posn-col p2)))
     'dleft]))

(test
 (posn-direction (make-posn 0 0) (make-posn 0 5)) => 'horiz
 (posn-direction (make-posn 0 0) (make-posn 5 0)) => 'vert
 (posn-direction (make-posn 0 0) (make-posn 5 5)) => 'dright
 (posn-direction (make-posn 5 5) (make-posn 0 0)) => 'uleft
 (posn-direction (make-posn 0 5) (make-posn 5 0)) => 'dleft
 (posn-direction (make-posn 5 0) (make-posn 0 5)) => 'uright)

(define (bad-posn? p)
  (or (negative? (posn-row p))
      (negative? (posn-col p))))

(define (posn-neighbors p)
  (filter-not
   bad-posn?
   (list* (posn-left (posn-up p))
          (posn-left (posn-down p))
          (posn-right (posn-up p))
          (posn-right (posn-down p))
          (posn-sq-neighbors p))))

(define (posn-sq-neighbors p)
  (filter-not
   bad-posn?
   (list (posn-up p)
         (posn-down p)
         (posn-left p)
         (posn-right p))))

(provide
 posn)
(provide/contract
 [rename make-posn* make-posn
         (exact-nonnegative-integer? exact-nonnegative-integer? . -> . posn?)]
 [posn? (any/c . -> . boolean?)]
 [posn-row (posn? . -> . exact-nonnegative-integer?)]
 [posn-col (posn? . -> . exact-nonnegative-integer?)]
 [make-valid-posn? (exact-nonnegative-integer? exact-nonnegative-integer? . -> . (posn? . -> . boolean?))]
 [posn-adjust (posn? exact-integer? exact-integer? . -> . posn?)]
 [posn=? (posn? exact-integer? exact-integer? . -> . boolean?)]
 [posn<= (posn? posn? . -> . boolean?)]
 [posn-left (posn? . -> . posn?)]
 [posn-right (posn? . -> . posn?)]
 [posn-up (posn? . -> . posn?)]
 [posn-down (posn? . -> . posn?)]
 [posn-direction (posn? posn? . -> . (symbols 'vert 'horiz 'dright 'dleft 'uleft 'uright))]
 [posn-distance (posn? posn? . -> . number?)]
 [posn-sq-neighbors (posn? . -> . (listof posn?))]
 [posn-neighbors (posn? . -> . (listof posn?))])
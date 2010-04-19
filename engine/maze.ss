#lang scheme
(require (planet jaymccarthy/matrix)
         "../lib/posn.ss"
         "../lib/random.ss")

(define-struct cell (visited? walls) #:transparent)
(define-struct maze (matrix) #:transparent)

(define (generate-maze rows cols)
  (define m
    (build-matrix rows cols (lambda (ri ci) (make-cell #f '(l r u d ul ur dl dr)))))
  (define valid-posn? (make-valid-posn? rows cols))
  (define (neighbors p)
    (filter valid-posn? (posn-neighbors p)))
  (define (visit! p)
    (matrix-set! m (posn-row p) (posn-col p)
                 (make-cell #t (cell-walls m p))))
  (define (remove-wall! p p-wall)
    (matrix-set! m (posn-row p) (posn-col p)
                 (make-cell #t (remq p-wall (cell-walls m p)))))
  (define (remove-walls! c p)
    (define-values (p-wall c-wall)
      (cond
        [(equal? (posn-left c) p)
         (values 'r 'l)]
        [(equal? (posn-right c) p)
         (values 'l 'r)]
        [(equal? (posn-up c) p)
         (values 'd 'u)]
        [(equal? (posn-down c) p)
         (values 'u 'd)]
        [(equal? (posn-left (posn-down c)) p)
         (values 'ur 'dl)]
        [(equal? (posn-right (posn-down c)) p)
         (values 'ul 'dr)]
        [(equal? (posn-left (posn-up c)) p)
         (values 'dr 'ul)]
        [(equal? (posn-right (posn-up c)) p)
         (values 'dl 'ur)]))
    (remove-wall! p p-wall)
    (remove-wall! c c-wall))
  (define (select-cell cur-cell)
    (define (select-neighbor n)
      (unless (cell-visited? m n)
        (remove-walls! cur-cell n)
        (select-cell n)))
    (visit! cur-cell)
    (for-each select-neighbor (permute-list (neighbors cur-cell))))
  (select-cell 
   (make-posn 
    (random rows)
    (random cols)))
  (make-maze m))

(define (render-maze mz)
  (define m (maze-matrix mz))
  (define rm 
    (build-matrix 
     (* 3 (matrix-rows m))
     (* 3 (matrix-cols m))
     (lambda (ri ci) "?")))
  (for ([ri (in-range 0 (matrix-rows m))])
    (for ([ci (in-range 0 (matrix-cols m))])
      (define w (cell-walls m (make-posn ri ci)))
      (define (setrm! rd cd wl)
        (matrix-set! rm (+ rd (* 3 ri)) (+ cd (* 3 ci))
                     (if (member wl w) "#" ".")))
      (setrm! 0 0 'ul)
      (setrm! 0 1 'u)
      (setrm! 0 2 'ur)
      (setrm! 1 0 'l)
      (setrm! 1 1 'not-a-wall)
      (setrm! 1 2 'r)
      (setrm! 2 0 'dl)
      (setrm! 2 1 'd)
      (setrm! 2 2 'dr)))
  rm)

(define (print-maze m)
  (display-matrix (render-maze m)))

(provide/contract
 [maze? (any/c . -> . boolean?)]
 [generate-maze (exact-positive-integer? exact-positive-integer? . -> . maze?)]
 [render-maze (maze? . -> . matrix?)])
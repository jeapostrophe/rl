#lang scheme
(require tests/eli-tester
         scheme/package)
(require "../lib/posn.ss")

#|
http://en.wikipedia.org/wiki/Bresenham's_line_algorithm

function line(x0, x1, y0, y1)
     boolean steep := abs(y1 - y0) > abs(x1 - x0)
     if steep then
         swap(x0, y0)
         swap(x1, y1)
     if x0 > x1 then
         swap(x0, x1)
         swap(y0, y1)
     int deltax := x1 - x0
     int deltay := abs(y1 - y0)
     real error := 0
     real deltaerr := deltay / deltax
     int ystep
     int y := y0
     if y0 < y1 then ystep := 1 else ystep := -1
     for x from x0 to x1
         if steep then plot(y,x) else plot(x,y)
         error := error + deltaerr
         if error ≥ 0.5 then
             y := y + ystep
             error := error - 1.0
|#
(define (posn-path p0 p1)
  (if (equal? p0 p1)
      empty
      (local
        [(define x0 (posn-col p0))
         (define y0 (posn-row p0))
         (define x1 (posn-col p1))
         (define y1 (posn-row p1))
         (define steep? ((abs (- y1 y0)) . > . (abs (- x1 x0))))]
        (cond
          [steep?
           (map (match-lambda
                  [(struct posn (y x)) (make-posn x y)])
                (posn-path (make-posn x0 y0) (make-posn x1 y1)))]
          [(x0 . > . x1)
           (posn-path p1 p0)]
          [else
           (local
             [(define deltax (- x1 x0))
              (define deltay (abs (- y1 y0)))
              (define deltaerr (/ deltay deltax))
              (define ystep 
                (if (y0 . < . y1)
                    1
                    -1))
              (define-values (path y error)
                (for/fold 
                    ([l empty] [y y0] [error 0]) ([x (in-range x0 x1)])
                  (define new-error (+ error deltaerr))
                  (define-values (next-y next-error)
                    (if (new-error . >= . 0.5)
                        (values (+ y ystep)
                                (- new-error 1))
                        (values y new-error)))
                  (values (list* (make-posn y x) l)
                          next-y
                          next-error)))]
             (rest (reverse path)))]))))

(test
 (posn-path (make-posn 0 0) (make-posn 0 0)) => 
 empty
 
 (posn-path (make-posn 0 0) (make-posn 0 5)) =>
 (list (make-posn 0 1) (make-posn 0 2) (make-posn 0 3) (make-posn 0 4))
 
 (posn-path (make-posn 0 0) (make-posn 5 0)) =>
 (list (make-posn 1 0) (make-posn 2 0) (make-posn 3 0) (make-posn 4 0))
 
 (posn-path (make-posn 0 0) (make-posn 5 5)) =>
 (list (make-posn 1 1) (make-posn 2 2) (make-posn 3 3) (make-posn 4 4))
 
 (posn-path (make-posn 1 1) (make-posn 5 11)) =>
 (list (make-posn 1 2) (make-posn 2 3) (make-posn 2 4) (make-posn 3 5) (make-posn 3 6) (make-posn 3 7)
       (make-posn 4 8) (make-posn 4 9) (make-posn 5 10)))

(define (potential-posns origin radius)
  (for*/list ([ri (in-range (* -1 radius) (add1 radius))]
              [ci (in-range (* -1 radius) (add1 radius))])
    (posn-adjust origin ri ci)))

(test 
 (potential-posns (make-posn 0 0) 0) =>
 (list (make-posn 0 0))
 
 (potential-posns (make-posn 1 1) 1) => 
 (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2)
       (make-posn 1 0) (make-posn 1 1) (make-posn 1 2)
       (make-posn 2 0) (make-posn 2 1) (make-posn 2 2))
 
 (potential-posns (make-posn 2 2) 2) => 
 (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2) (make-posn 0 3) (make-posn 0 4)
       (make-posn 1 0) (make-posn 1 1) (make-posn 1 2) (make-posn 1 3) (make-posn 1 4)
       (make-posn 2 0) (make-posn 2 1) (make-posn 2 2) (make-posn 2 3) (make-posn 2 4)
       (make-posn 3 0) (make-posn 3 1) (make-posn 3 2) (make-posn 3 3) (make-posn 3 4)
       (make-posn 4 0) (make-posn 4 1) (make-posn 4 2) (make-posn 4 3) (make-posn 4 4)))

(define (valid-posns posn-valid? origin radius)
  (filter posn-valid?
          (potential-posns origin radius)))

(test 
 (valid-posns (make-valid-posn? 2 2) (make-posn 0 0) 0) =>
 (list (make-posn 0 0))
 
 (valid-posns (make-valid-posn? 2 2) (make-posn 1 1) 1) => 
 (list (make-posn 0 0) (make-posn 0 1)
       (make-posn 1 0) (make-posn 1 1))
 
 (valid-posns (make-valid-posn? 5 5) (make-posn 2 2) 2) => 
 (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2) (make-posn 0 3) (make-posn 0 4)
       (make-posn 1 0) (make-posn 1 1) (make-posn 1 2) (make-posn 1 3) (make-posn 1 4)
       (make-posn 2 0) (make-posn 2 1) (make-posn 2 2) (make-posn 2 3) (make-posn 2 4)
       (make-posn 3 0) (make-posn 3 1) (make-posn 3 2) (make-posn 3 3) (make-posn 3 4)
       (make-posn 4 0) (make-posn 4 1) (make-posn 4 2) (make-posn 4 3) (make-posn 4 4)))

(define (posns-within-radius posn-valid? origin radius)
  (filter (lambda (p)
            ((posn-distance origin p) . <= . radius))
          (valid-posns posn-valid? origin radius)))

(test 
 (posns-within-radius (make-valid-posn? 1 1) (make-posn 0 0) 0) =>
 (list (make-posn 0 0))
 
 (posns-within-radius (make-valid-posn? 3 3) (make-posn 1 1) 1) => 
 (list                 (make-posn 0 1) 
       (make-posn 1 0) (make-posn 1 1) (make-posn 1 2)
                       (make-posn 2 1))
 
 (posns-within-radius (make-valid-posn? 5 5) (make-posn 2 2) 2) => 
 (list                                 (make-posn 0 2)
                       (make-posn 1 1) (make-posn 1 2) (make-posn 1 3)
       (make-posn 2 0) (make-posn 2 1) (make-posn 2 2) (make-posn 2 3) (make-posn 2 4)
                       (make-posn 3 1) (make-posn 3 2) (make-posn 3 3)
                                       (make-posn 4 2)))

(define ((clear-path-to-origin? obstruction? origin) p)
  (andmap (compose not obstruction?)
          (posn-path origin p)))

(package-begin
 (define (string-map->obstruction? s)
  (define rows (regexp-split #rx"\n" s))
  (lambda (p)
    (char=? #\# (string-ref (list-ref rows (posn-row p)) (posn-col p)))))
 (define test-map0
   #<<END
.......
....#..
..@.#..
.......
END
   )
 (define test-map0-obstruction? (string-map->obstruction? test-map0))
 (define clear-path?/map0 (clear-path-to-origin? test-map0-obstruction? (make-posn 2 2)))
 (test
  (clear-path?/map0 (make-posn 0 0)) => #t
  (clear-path?/map0 (make-posn 2 0)) => #t
  (clear-path?/map0 (make-posn 2 6)) => #f
  (clear-path?/map0 (make-posn 0 6)) => #f
  (clear-path?/map0 (make-posn 1 6)) => #f
  (clear-path?/map0 (make-posn 2 6)) => #f
  (clear-path?/map0 (make-posn 3 6)) => #t)
 
 (define test-map1
   #<<END
0123456
1.@....
2.#....
3.#....
END
   )
 (define test-map1-obstruction? (string-map->obstruction? test-map1))
 (define clear-path?/map1 (clear-path-to-origin? test-map1-obstruction? (make-posn 1 2)))
 (test
  (clear-path?/map1 (make-posn 2 2)) => #t
  (clear-path?/map1 (make-posn 3 2)) => #f
  (clear-path?/map1 (make-posn 3 1)) => #t
 ))

(define (simple-update-fov posn-valid? obstruction? origin radius)
  (filter (clear-path-to-origin? obstruction? origin)
          (posns-within-radius posn-valid? origin radius)))

(define (update-fov max-rows max-cols posn-valid? obstruction? origin radius)
  (simple-update-fov posn-valid? obstruction? origin radius))

(provide/contract
 [update-fov (exact-positive-integer?
              exact-positive-integer?
              (posn? . -> . boolean?)
              (posn? . -> . boolean?)
              posn?
              exact-nonnegative-integer?
              . -> .
              (listof posn?))])
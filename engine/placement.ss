#lang scheme
; Based on http://roguebasin.roguelikedevelopment.org/index.php?title=Creating_Measurably_%22Fun%22_Maps
(require "../lib/tree.ss"
         "../lib/posn.ss"
         "../lib/random.ss"
         "../copy-obj/this-class.ss"
         "tile.ss"
         "obj.ss"
         "ai.ss"
         "map.ss")

(define (mob-posn map0)
  (send map0 random-posn
        (lambda (p) 
          (define-values (ok? msg) (send (send map0 tile p) enter?))
          ok?)))

(define (create&place 
         map0 class% 
         [ip (mob-posn map0)])
  (define obj (new class% [posn (make-posn 0 0)]))
  (define map1
    (send map0 update-tile ip 
          (send (send map0 tile ip) move obj)))
  (values (send (send obj move-floor 0 map1)
                move-posn map1 ip)
          map1))

(define (walkable-posns map ps)
  (filter (lambda (p)
            (is-a? (send map tile p) content-tile%))
          ps))
(define (walkable-neighbors map p)
  (walkable-posns map (posn-neighbors p)))
(define (all-posns-of map)
  (for*/list ([ri (in-range 0 (send map max-row))]
              [ci (in-range 0 (send map max-col))])
    (make-posn ri ci)))

(require (planet dherman/set:4/set)
         "../lib/connected.ss")
(define (map-divided-by-path map path)
  (define path-nodes 
    (list->set path))
  (define (not-on-path-nodes ps)
    (filter 
     (lambda (p)
       (not (set-contains? path-nodes p)))
     ps))
  (define all-posns
    (not-on-path-nodes
     (walkable-posns 
      map 
      (all-posns-of map))))
  (define ccs
    (connected-components
     all-posns
     (lambda (p)
       (not-on-path-nodes
        (walkable-neighbors map p)))
     (lambda (p) p)))
  (define inter
    (set-intersections ccs))
  (unless (set-empty? inter)
    (error 'map-divided-by-path "connected-components is broken: intersection ~S~n" inter))
  ccs)

(define (argmin/score f l)
  (match
      (argmin
       cdr
       (map (lambda (o)
              (cons o (f o))) 
            l))
    [(list-rest o score)
     (values o score)]))

(require "../lib/astar.ss"
         (planet dherman/memoize))
(define (map-shortest-paths mmap)
  (define/memo* (map-shortest-path p0 p1)
    (printf "~S to ~S~n" p0 p1)
    (a* (lambda (x y) 1)
        posn-distance
        (lambda (p) (walkable-neighbors mmap p))
        p0 p1))
  map-shortest-path)

(define (index e l)
  (let loop ([i 0] [l l])
    (if (empty? l)
        (error 'index "~a not in list ~a~n" e l)
        (if (equal? e (first l))
            i
            (loop (add1 i) (rest l))))))    

#|
(require "../lib/floyd-warshall.ss"
         (planet wmfarr/simple-matrix/matrix-base))
(define (map-shortest-paths mmap)
  (define all-posns (all-posns-of mmap))
  (define all-posns/v (list->vector all-posns))
  (define m
    (shortest-paths 
     (sub1 (length all-posns))
     (lambda (i j)
       (if (= i j)
           (make-path 0 empty)
           (local
             [(define i-posn (vector-ref all-posns/v i))
              (define j-posn (vector-ref all-posns/v j))]
             (if (= (posn-distance i-posn j-posn) 1)
                 (make-path 1 (list j-posn))
                 (make-path +inf.0 empty)))))))
  (lambda (p0 p1)
    (path-edges (matrix-ref m (index p0 all-posns) (index p1 all-posns)))))
|#

(define THRESHOLD 10)
(define (place mmap)
  (define map-shortest-path (map-shortest-paths mmap))
  ; XXX This should compute a in-toto shortest path
  (define (map-shortest-path* start ps)
    (match ps
      [(list)
       (list start)]
      [(list-rest next-p ps)
       (append (map-shortest-path start next-p)
               (map-shortest-path* next-p ps))]))
  #;(define (map-shortest-path* start ps)
      (match ps
        [(list)
         (list start)]
        [_
         (define next-p
           (argmin (lambda (p)
                     (length (map-shortest-path start p)))
                   ps))
         (append (map-shortest-path start next-p)
                 (map-shortest-path* next-p (remq next-p ps)))]))
  
  (define start-posn (mob-posn mmap))
  (define end-posn (mob-posn mmap))  
  (if (equal? start-posn end-posn)
      (place mmap)
      (local [(define-values
                (points path)
                (let loop ([points (list end-posn)])
                  (local [(define path (map-shortest-path* start-posn points))
                          (define pointless-areas (map-divided-by-path mmap path))
                          (define largest-pointless (argmax set-count pointless-areas))]
                    (unless (andmap (lambda (p) (member p path)) (list* start-posn points))
                      (error 'place "Not every point (~S) is on the path: ~S~n"
                             (list* start-posn points)
                             path))                    
                    (printf "~S~n" (map set-count pointless-areas))
                    (if ((set-count largest-pointless) . > . THRESHOLD)
                        (local [(define random-point 
                                  (argmax (lambda (rp) 
                                            (define-values (pp sc)
                                              (argmin/score
                                               (lambda (pp) 
                                                 ; XXX This should compute a shortest path
                                                 (posn-distance pp rp))
                                               path))
                                            sc)
                                          (set->list largest-pointless)))]
                          (when (member random-point points)
                            (error 'place "Point already in! ~S~n" random-point))
                          (loop (list* random-point points)))
                        (values points path)))))
              ; XXX Use these points to put obstacles/rewards/etc
              (define paren-map
                (for/fold ([paren-map mmap])
                  ([path-posn points])
                  (define pc (random-element (list 'red 'green 'yellow 'blue 'magenta 'cyan)))
                  (define dir (random-element (list 'left 'right)))
                  (define tile (send paren-map tile path-posn))
                  (define paren (new paren% [color pc] [orient dir]))
                  (if (send tile can-drop?)
                      (send paren-map update-tile path-posn
                            (send tile drop paren))
                      paren-map)))]
        (values start-posn paren-map empty))))

(provide/contract
 [create&place 
  (((is-a?/c map<%>) 
    class?)
   (posn?)
   . ->* .
   (values object?
           (is-a?/c map<%>)))]
 [place ((is-a?/c map<%>) 
         . -> . 
         (values posn?
                 (is-a?/c map<%>)
                 (listof object?)))])
#lang scheme
(require "../lib/posn.ss"        
         "../lib/matrix-posn.ss"
         "../lib/retry.ss"
         "../copy-obj/this-class.ss"
         "maze.ss"
         "tile.ss")

(define map<%>
  (interface ()
    max-row max-col
    tile valid-tile?
    random-posn))

(define matrix-map%
  (class%* object% (map<%>)
    (init-slot matrix)
        
    (define/public (max-row) (matrix-rows matrix))    
    (define/public (max-col) (matrix-cols matrix))    
    (init-slot 
     [valid?
      (make-valid-posn? (matrix-rows matrix) (matrix-cols matrix))])
    
    (define/public (valid-tile? p)
      (valid? p))
    (define/public (tile p)
      (matrix-ref/posn matrix p))
    (define/public (update-tile p t)
      (copy [matrix (matrix-set/posn matrix p t)]))
    
    (define/public (random-posn okay?)
      (retry-monad
       +inf.0 #f
       [r (random (matrix-rows matrix))]
       [c (random (matrix-cols matrix))]
       [gp (make-posn r c)]
       [gp+ (if (okay? gp) gp #f)]))
    
    (super-new)))

(define (random-map map-rows map-cols)
  (new matrix-map%
       [matrix
        (build-matrix 
         map-rows map-cols
         (lambda (r c) 
           (if (zero? (random 6))
               (new wall%)
               (new floor%))))]))

(define (empty-map map-rows map-cols)
  (new matrix-map%
       [matrix
        (build-matrix 
         map-rows map-cols
         (lambda (r c) 
           (new floor%)))]))

(define (maze-map map-rows map-cols)
  (define maze 
    (render-maze 
     (generate-maze 
      (floor (/ map-rows 3))
      (floor (/ map-cols 3)))))
  (new matrix-map%
       [matrix
        (build-matrix 
         map-rows map-cols
         (lambda (r c)
           (match (with-handlers ([exn:fail? (lambda _ #f)])
                    (matrix-ref maze r c))
             ["#" (new wall%)]
             ["." (new floor%)]
             [#f (new wall%)])))]))

(provide/contract
 [map<%> interface?]
 [matrix-map% (implementation?/c map<%>)]
 [random-map (exact-positive-integer? exact-positive-integer? . -> . (is-a?/c map<%>))]
 [empty-map (exact-positive-integer? exact-positive-integer? . -> . (is-a?/c map<%>))]
 [maze-map (exact-positive-integer? exact-positive-integer? . -> . (is-a?/c map<%>))])
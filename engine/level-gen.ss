#lang scheme
; Based on http://roguebasin.roguelikedevelopment.org/index.php?title=Dungeon-Building_Algorithm
(require "../lib/matrix-posn.ss"
         "../lib/posn.ss"
         "../lib/prob.ss"
         "../lib/retry.ss"
         "../lib/random.ss"
         "../copy-obj/this-class.ss"
         "map.ss"
         "tile.ss")
(require tests/eli-tester)

(define feature<%>
  (interface ()
    dig join))

(define feature%
  (class* object% (feature<%>)
    (define/public (dig m)
      #f)
    (define/public (join m1 m2)
      #f)
    (super-new)))

(define tunnel-feature%
  (class* feature% (feature<%>)
    (init-field start-row start-col width height [overlap-okay? #t])
    
    (define/override (dig m)
      (for*/fold ([m m])
        ([hi (in-range 0 height)]
         [wi (in-range 0 width)])
        (if m
            (local [(define col (+ start-col wi))
                    (define row (+ start-row hi))
                    (define cell (matrix-ref* m row col))]
              (and cell
                   (cond
                     [(and overlap-okay? (is-a? cell tunnel%))
                      m]
                     [(is-a? cell empty%)
                      (matrix-set m row col (new tunnel%))]
                     [else
                      #f])))
            #f)))
    
    ; XXX This doesn't work well
    (define/override (join m1 m2)
      (if
       (and overlap-okay?
       (for*/or ([hi (in-range 0 height)]
                 [wi (in-range 0 width)])
         (local [(define col (+ start-col wi))
                 (define row (+ start-row hi))
                 (define cell (matrix-ref* m1 row col))]
           (and cell
                (is-a? cell tunnel%)))))
       m2
       ; XXX This only finds walls, not tunnels two away
       (local [(define perimeter-posns/top
                 (for*/list ([wi (in-range 0 width)])
                   (make-posn* (sub1 start-row) (+ start-col wi))))
               (define perimeter-posns/bottom
                 (for*/list ([wi (in-range 0 width)])
                   (make-posn* (+ start-row height) (+ start-col wi))))
               (define perimeter-posns/left
                 (for*/list ([hi (in-range 0 height)])
                   (make-posn* (+ start-row hi) (sub1 start-col))))
               (define perimeter-posns/right
                 (for*/list ([hi (in-range 0 height)])
                   (make-posn* (+ start-row hi) (+ start-col width))))
               (define candidates
                 (for/fold ([candidates empty])
           ([p (in-sequences (in-list perimeter-posns/top)
                             (in-list perimeter-posns/bottom)
                             (in-list perimeter-posns/left)
                             (in-list perimeter-posns/right))])
           (if (and p (non-corner-wall? m1 (posn-row p) (posn-col p)))
               (list* p candidates)
               candidates)))]
         (if (empty? candidates)
             #f
             ; XXX Pick a random number of these
             (matrix-set/posn m2 (random-element candidates) (new door%))))))
    
    (super-new)))

(define room-feature%
  (class* feature% (feature<%>)
    (init-field corner-row corner-col width height)
    
    (define (top? wi hi) (= hi 0))
    (define (bottom? wi hi) (= hi (sub1 height)))
    (define (left? wi hi) (= wi 0))
    (define (right? wi hi) (= wi (sub1 width)))
    (define (wall? wi hi)
      (or (top? wi hi) (bottom? wi hi) (left? wi hi) (right? wi hi)))
    (define (corner? wi hi)
      (or (and (top? wi hi) (or (left? wi hi) (right? wi hi)))
          (and (bottom? wi hi) (or (left? wi hi) (right? wi hi)))))
    
    (define/override (dig m)
      (for*/fold ([m m])
        ([wi (in-range 0 width)]
         [hi (in-range 0 height)])
        (if m
            (local
              [(define row (+ corner-row hi))
               (define col (+ corner-col wi))
               (define current (matrix-ref* m row col))]
              (and current
                   (cond
                     [(is-a? current wall%)
                      (if (wall? wi hi)
                          m
                          #f)]
                     #;[(is-a? current tunnel%)
                        (if (wall? wi hi)
                            m
                            #f)]
                     [(is-a? current floor%)
                      (if (wall? wi hi)
                          #f
                          m)]
                     [(is-a? current empty%)
                      (matrix-set m row col
                                  (cond
                                    [(wall? wi hi)
                                     (new wall%)]
                                    [else (new floor%)]))]
                     [else
                      #f])))
            #f)))
    
    (define walls
      (map (match-lambda
             [(list-rest wi hi)
              (cons (+ corner-col wi)
                    (+ corner-row hi))])
           (filter (match-lambda
                     [(list-rest wi hi)
                      (and (wall? wi hi) (not (corner? wi hi)))])
                   (for*/list ([wi (in-range 0 width)]
                               [hi (in-range 0 height)])
                     (cons wi hi)))))
    
    (define/override (join m1 m2)
      (define shared-walls 
        (filter 
         (match-lambda 
           [(list-rest wi hi)
            (or (is-a? (matrix-ref m1 hi wi) tunnel%)
                (non-corner-wall? m1 hi wi))])
         walls))
      (if (empty? shared-walls)
          #f
          (local [(define random-walls
                    (permute-list shared-walls))
                  (define how-many-walls
                    (length random-walls))
                  (define how-many-doors
                    (random-in 1 (add1 (random (ceiling (/ how-many-walls 2))))))]
            (for/fold ([m2 m2])
              ([i (in-list (list-tail random-walls (- how-many-walls how-many-doors)))])
              (define door-p (make-posn (cdr i) (car i)))
              (if (ormap (lambda (p)
                           (is-a? (matrix-ref*/posn m2 p) door%))
                         (posn-neighbors door-p))
                  m2
                  (matrix-set/posn m2 door-p (new door%)))))))
    
    (super-new)))

(define (make-posn* h w)
  (and (>= h 0) (>= w 0)
       (make-posn h w)))

(define (matrix-ref* m h w)
  (and (>= h 0) (>= w 0)
       (matrix-valid-ref? m h w)
       (matrix-ref m h w)))

(define (matrix-ref*/posn m p)
  (define h (posn-row p))
  (define w (posn-col p))
  (and (>= h 0) (>= w 0)
       (matrix-valid-ref? m h w)
       (matrix-ref m h w)))

(define (non-corner-wall? m1 hi wi)
  (define p00 (matrix-ref* m1 hi wi))
  (define p10 (matrix-ref* m1 (add1 hi) wi))
  (define p01 (matrix-ref* m1 hi (add1 wi)))
  (define p-10 (matrix-ref* m1 (sub1 hi) wi))
  (define p0-1 (matrix-ref* m1 hi (sub1 wi)))
  (define (wall? c)
    (or (is-a? c wall%)
        (is-a? c door%)))
  (and 
   p00
   (wall? p00)
   (not (and p10 p01 (wall? p10) (wall? p01)))
   (not (and p10 p0-1 (wall? p10) (wall? p0-1)))
   (not (and p-10 p01 (wall? p-10) (wall? p01)))
   (not (and p-10 p0-1 (wall? p-10) (wall? p0-1)))))

(define (wall? c)
  (is-a? c wall%))

(define (generate rows cols)
    (define m (build-matrix rows cols (lambda (i j) (new empty%))))
    (define m1
      (send (new room-feature%
                 [corner-row (round (/ (random rows) 2))]
                 [corner-col (round (/ (random cols) 2))]
                 [width (random-in 4 4)]
                 [height (random-in 4 4)])
            dig m))
    ; XXX More kinds of features and configurations
    (for/fold ([m1 m1])
      ([feature_i (in-range 0 50)])
      (retry-monad
       +inf.0 m1
       [feature 
        (with-probability
         [1
          (new room-feature%
               [corner-row (random rows)]
               [corner-col (random cols)]
               [width (random-in 4 8)]
               [height (random-in 4 8)])]
         [0
          (new tunnel-feature%
               [start-row (random rows)]
               [start-col (random cols)]
               [height 1]
               [width (random-in 3 10)])]
         [0
          (new tunnel-feature%
               [start-row (random rows)]
               [start-col (random cols)]
               [width 1]
               [height (random-in 3 10)])])]
       [m2 (send feature dig m1)]
       [m3 (send feature join m1 m2)])))

(define (generate-map rows cols)
  (define m (generate rows cols))
  (display-matrix m)
  (new matrix-map%
       [matrix m]))

(define (display-matrix m)
  (printf "  \t")
  (for ([col (in-range 0 (matrix-cols m))])
    (display (floor (/ col 10))))
  (printf "~n")
  (printf "  \t")
  (for ([col (in-range 0 (matrix-cols m))])
    (display (modulo col 10)))
  (printf "~n")    
  (for ([row (in-range 0 (matrix-rows m))])
    (printf "~a\t" row)
    (for ([col (in-range 0 (matrix-cols m))])
      (define cell (matrix-ref m row col))
      (define char 
        (cond
          [(is-a? cell empty%) #\space]
          [(is-a? cell tunnel%) #\=]
          [(is-a? cell floor%) #\.]
          [(is-a? cell wall%) #\#]
          [(is-a? cell door%) #\D]
          [else #\?]))
      (display char))
    (newline)))

(provide/contract
 [generate-map (exact-positive-integer? exact-positive-integer? . -> . (is-a?/c map<%>))])
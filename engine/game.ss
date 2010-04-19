#lang scheme
(require "../lib/tree.ss"
         "../lib/posn.ss"
         "../copy-obj/this-class.ss"
         "placement.ss"
         "level-gen.ss"
         "timed.ss"
         "mob.ss"
         "char.ss"
         "ai.ss"
         "tile.ss"
         "obj.ss"
         "map.ss")

(define game<%>
  (interface ()
    increment-game-time game-time
    get-char update-char
    update-map
    tick))

(define game%
  (class%* object% (game<%>)
    (init-slot [time 0] map objs)
    
    (define/public (increment-game-time)
      (copy [time (add1 time)]))
    (define/public (game-time)
      time)
    
    (define/public (update-map new-map)
      (copy [map new-map]))
    
    (define/public (get-char)
      (findf (lambda (o) (is-a? o char%))
             (timed-elements objs)))
    (define/public (update-char new-char)
      (copy 
       [objs 
        (timed-map 
         (lambda (o)
           (if (is-a? o char%)
               new-char
               o))
         objs)]))
    
    (define/public (tick)
      (match (progress objs this)
        [#f #f]
        [(struct progress-result (new-objs new-this updated-posns))
         (define changed updated-posns)
         (define final-this
           (send new-this update-objs 
                 (timed-map (lambda (a)
                              ; XXX Generalize this to be a general event delivery mechanism for everyone
                              (if (is-a? a char%)
                                  (let-values ([(a-changed new-a) (send a look (get-field map new-this))])
                                    (set! changed (tree-union changed a-changed))
                                    new-a)
                                  a))
                            new-objs)))
         (cons changed final-this)]))
    
    (define/public (update-objs new-objs)
      (copy [objs new-objs]))
    
    (super-new)))

(define clock%
  (timed-mixin
   (class%* object% (timed<%>)            
     (define/public (speed) NORMAL-SPEED)
     (define/public (activate game)
       (make-activate-result 
        NORMAL-SPEED this
        (send game increment-game-time)
        tree-empty empty))
     
     (super-new))))

(define (initial-game #:rows map-rows 
                      #:cols map-cols)
  (define map0
    (generate-map map-rows map-cols))
  (define the-clock (new clock%))
  (define-values (char-posn map2 ps)
    (place map0))
  (define-values (char map3) (create&place map2 char% char-posn))
  (define qi (make-timed-objects char))
  (define qj
    (for/fold ([q qi]) ([p (in-list ps)])
      (register-timed-object q p)))
  (define the-game
    (new game%
         [map map3]
         [objs
          (register-timed-object
           qj the-clock)]))  
  the-game)

(provide/contract
 [initial-game (#:rows exact-positive-integer? #:cols exact-positive-integer? . -> . (is-a?/c game%))])
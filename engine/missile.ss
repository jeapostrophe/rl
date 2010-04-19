#lang scheme
(require "timed.ss"
         "action.ss"
         "tile.ss"
         "../lib/tree.ss"
         "../copy-obj/this-class.ss")

; XXX Should be in air, not on ground
(define missile%
  (timed-mixin
   (class%* object% (timed<%>)
     ; XXX Base strength on thrower
     (init-slot obj posn dir [strength 5])
     
     ; XXX Base speed on something real
     (define/public (speed) (* 10 NORMAL-SPEED))
     (define/public (activate game)
       (if (zero? strength)
           (make-activate-result
            NO-SPEED #f game (tree-node posn) empty)
           (local [(define next-posn (direction->posn dir posn))
                   (define map (get-field map game))]
             (if (send map valid-tile? next-posn)
                 (local [(define cur-tile (send map tile posn))
                         (define next-tile (send map tile next-posn))]
                   ; XXX make a function call or interface test (won't work with air either)
                   (if (and (is-a? next-tile content-tile%)
                            (send next-tile can-drop?))
                       ; XXX Deal with collision with actors
                       (local [(define map1
                                 (send map update-tile posn
                                       (send cur-tile pickup obj)))
                               (define map2
                                 (send map1 update-tile next-posn
                                       (send next-tile drop obj)))
                               (define next-game
                                 (send game update-map map2))
                               (define new-this
                                 (copy [posn next-posn]
                                       [strength (sub1 strength)]))]
                         (make-activate-result 
                          NORMAL-SPEED new-this next-game
                          (tree-union (tree-node posn)
                                      (tree-node next-posn))
                          empty))
                       (make-activate-result
                        NO-SPEED #f game tree-empty empty)))
                 (make-activate-result
                  NO-SPEED #f game tree-empty empty)))))
     
     (super-new))))

(provide/contract
 [missile%
  (implementation?/c timed<%>)])
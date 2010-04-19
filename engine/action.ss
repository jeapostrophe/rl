#lang scheme
(require "../lib/posn.ss")

(define-struct action ())

(define directions
  '(up down left right left-up left-down right-up right-down))
(define direction/c
  (apply symbols directions))

(define-struct (move action) (direction))
(define-struct (drop action) (obj))
(define-struct (pickup action) (obj))
(define-struct (open action) (direction))
(define-struct (close action) (direction))
(define-struct (throw action) (obj direction))

(define (direction->posn direction p)
  (case direction
    [(up) (posn-up p)]
    [(down) (posn-down p)]
    [(left) (posn-left p)]
    [(right) (posn-right p)]
    [(left-up) (posn-adjust p -1 -1)]
    [(left-down) (posn-adjust p 1 -1)]
    [(right-up) (posn-adjust p -1 1)]
    [(right-down) (posn-adjust p 1 1)]))

(provide/contract
 [action? (any/c . -> . boolean?)]
 [struct move ([direction direction/c])]
 [directions (listof symbol?)]
 [direction->posn (direction/c posn? . -> . posn?)]
 [struct open ([direction direction/c])]
 [struct close ([direction direction/c])]
 ; XXX improve contracts
 [struct throw ([obj object?] [direction direction/c])]
 [struct drop ([obj object?])]
 [struct pickup ([obj object?])])
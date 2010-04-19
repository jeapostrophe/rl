#lang scheme
(require "../copy-obj/this-class.ss")

(define tile<%>
  (interface () 
    move move-away
    obstruction?
    render))

(define basic-tile%
  (class%* object% (tile<%>)
    (define/public (enter?) (values #f "You cannot enter a basic tile"))
    (define/public (leave?) (error 'leave? "Can't enter"))
    (define/public (move obj) (error 'move "Not implemented"))
    (define/public (move-away obj) (error 'move-away "Not implemented"))
    
    (define/public (obstruction?) #t)
    
    (define/public (on-tile) empty)
    (define/public (in-tile) #f)
    (define/public (render) 'empty)
    
    (super-new)))

(define empty%
  (class%* basic-tile% (tile<%>)
    (define/override (enter?) (values #f "You bonk your head."))
    (super-new)))

(define content-tile%
  (class%* basic-tile% (tile<%>)
    (init-slot [occupying-tile #f]
               [on-floor empty])
    
    (define/override (obstruction?) #f)
    
    (define/override (move obj)
      (if occupying-tile
          (error 'move "There is someone there")
          ; XXX Dangerous object duplication
          (copy [occupying-tile obj])))
    (define/override (move-away obj)
      ; XXX
      (unless (equal? obj occupying-tile)
        (log-warning (format  "Object is not in tile: ~S is, not ~S" occupying-tile obj)))
      (copy [occupying-tile #f]))
    
    (define/override (enter?)
      (if occupying-tile
          (values #f "There is something there")
          (values #t "")))
    (define/override (leave?) (values #t ""))
    
    (define/public (can-drop?)
      #t)
    ; XXX Potential danger since unchecked
    (define/public (drop obj)
      (copy [on-floor (list* obj on-floor)]))
    (define/public (pickup obj)
      (copy [on-floor (remq obj on-floor)]))
    
    (define/override (on-tile) on-floor)
    (define/override (in-tile) occupying-tile)
    
    
    (super-new)))

(define tunnel%
  (class%* content-tile% (tile<%>)
    (define/override (render) 'tunnel)
    
    (super-new)))

(define floor%
  (class%* content-tile% (tile<%>)
    (define/override (render) 'floor)
    
    (super-new)))

(define openable<%>
  (interface ()
    open close))

(define door%
  (class%* content-tile% (tile<%> openable<%>)
    (init-slot [open? #f])
    (define/override (render) 
      (if open?
          'open-door
          'closed-door))
    (define/override (enter?)
      (if open?
          (super enter?)
          (values #f "The door is closed.")))
    (define/override (obstruction?) (not open?))

    ; XXX Incorporate stuck-ness and strength of actor
    (inherit-field on-floor)
    (define/public (can-open?) (not open?))
    (define/public (open) (copy [open? #t]))
    (define/public (can-close?)
      (and open? (empty? on-floor)))
      
    (define/public (close) (copy [open? #f]))    
    
    (define/override (can-drop?)
      open?)
    
    (super-new)))

(define wall%
  (class%* basic-tile% (tile<%>)
    (define/override (enter?) (values #f "You bonk your head."))
    
    (define/override (render) 'wall)
    
    (super-new)))

(provide/contract
 [tile<%> interface?]
 [openable<%> interface?]
 [content-tile% (implementation?/c tile<%>)]
 [empty% (implementation?/c tile<%>)]
 [tunnel% (implementation?/c tile<%>)]
 [door% (implementation?/c tile<%>)]
 [floor% (implementation?/c tile<%>)]
 [wall% (implementation?/c tile<%>)])
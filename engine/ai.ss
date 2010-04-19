#lang scheme
(require "../lib/random.ss"
         "../lib/posn.ss"
         "../lib/tree.ss"
         "../copy-obj/this-class.ss"
         "action.ss"
         "mob.ss")

(define ai<%> 
  (interface (mob<%>) 
    receive-message))

(define pythonista%
  (class%* mob% (ai<%>)
    (define/override (short-desc)
      "Rabid Pythonista")
    
    (inherit-field posn)
    (inherit current-floor-map)
    (define/override (next-action)
      (define map (current-floor-map))
      (cons 
       this
       (make-move 
        (random-element
         (for/fold ([options empty])
           ([d directions]
            [np (posn-neighbors posn)])
           (define-values
             (enter? msg)
             (send (send map tile np) enter?))
           (if enter?
               (list* d options)
               options))))))
    
    (define/public (receive-message m) this)
    (define/override (look map) (values tree-empty this))
    
    (define/override (render)
      'pythonista)
    
    (super-new)))

(provide/contract
 [ai<%> interface?]
 [pythonista% (implementation?/c ai<%>)])

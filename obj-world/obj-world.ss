#lang scheme

(define world<%>
  (interface ()
    height
    width
    on-tick
    on-key
    on-draw
    stop-when
    stop-timer))

(define world%
  (class* object% (world<%>)
    (define/public (height) 24)
    (define/public (width) 80)
    (define/public (on-tick) this)
    (define/public (on-key key) this)
    (define/public (on-draw sc) (error 'on-draw "Not implemented"))
    (define/public (stop-when) #f)
    (define/public (stop-timer) #f)
    
    (super-new)))

(provide world<%>
         world%)
#lang scheme
(require (planet wmfarr/simple-matrix/matrix-base)
         "text-world-sig.ss")

(define cache-text-world@
  (unit
    (import (prefix inner: text-world^))
    (export text-world^)
    
    (define inner-scene #f)
    
    (define scene? matrix?)
    
    (define image? inner:image?)
    (define (place-image! img x y s)
      (define c (matrix-ref s y x))
      ; Note: Relies of equal? of images
      (unless (and c (equal? c img))
        (matrix-set! s y x img)
        (inner:place-image! img x y inner-scene)))
    
    (define (place-image*! str x y fc bc s)
      ; XXX Cache
      (inner:place-image*! str x y fc bc inner-scene))
    
    (define make-image inner:make-image)
    
    (define (big-bang init 
                      #:height height
                      #:width width
                      #:on-tick the-on-tick
                      #:tick-rate tick-rate
                      #:on-key the-on-key
                      #:on-draw the-on-draw
                      #:stop-when the-stop-when
                      #:stop-timer the-stop-timer)
      (define cached-scene
        (for/matrix height width ([i (in-range (* height width))]) #f))
      (inner:big-bang init 
                      #:height height
                      #:width width
                      #:on-tick the-on-tick
                      #:tick-rate tick-rate
                      #:on-key the-on-key
                      #:on-draw 
                      (lambda (w s)
                        (set! inner-scene s)
                        (the-on-draw w cached-scene))
                      #:stop-when the-stop-when
                      #:stop-timer the-stop-timer))))

(define (make-cache-text-world@ inner-world@)
  (compound-unit
    (import)
    (export CTW)
    (link [((IW : text-world^)) inner-world@]
          [((CTW : text-world^)) cache-text-world@ IW])))

(provide/contract
 [make-cache-text-world@ ((unit/c (import) (export text-world^)) . -> . (unit/c (import) (export text-world^)))])
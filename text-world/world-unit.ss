#lang scheme
(require (prefix-in uni: 2htdp/universe)
         (prefix-in img: lang/private/imageeq)
         "text-world-sig.ss")

(define (make-world-text-world@ text-size)
  (unit
    (import)
    (export text-world^)
    
    (define char-img (uni:text "@" text-size "black"))
    (define cell-width (uni:image-width char-img))
    (define cell-height (uni:image-height char-img))
    
    (define scene? (box/c uni:scene?))
    (define (empty-scene width height)
      (uni:place-image
       (uni:nw:rectangle
        (* width cell-width) (* height cell-height)
        'solid
        "black")
       0 0
       (uni:empty-scene (* width cell-width) (* height cell-height))))
    
    (define image? img:image?)
    (define (place-image! img x y s)
      (set-box! 
       s
       (uni:place-image 
        img
        (* cell-width x)
        (* cell-height y)
        (unbox s))))
    
    (define (make-image char fcolor bcolor)
      (uni:overlay
       (uni:nw:rectangle
        (* 1 cell-width) (* 1 cell-height)
        'solid
        bcolor)
       (uni:text (string char) text-size fcolor)))
    
    (define (big-bang init 
                      #:height height
                      #:width width
                      #:on-tick the-on-tick
                      #:tick-rate tick-rate
                      #:on-key the-on-key
                      #:on-draw the-on-draw
                      #:stop-when the-stop-when)
      (uni:big-bang init
                    (uni:on-tick the-on-tick tick-rate)
                    (uni:on-key the-on-key)
                    (uni:on-draw (lambda (w) 
                                   (define sc (box (empty-scene width height)))
                                   (the-on-draw w sc)
                                   (unbox sc)))
                    (uni:stop-when the-stop-when)))))

(provide/contract
 [make-world-text-world@ (exact-positive-integer? . -> . (unit/c (import) (export text-world^)))])
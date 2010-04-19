#lang scheme/gui
(require htdp/image
         "text-world-sig.ss")

; Note: This looks really bad, but is kind of useful
(define-syntax-rule (with-busy-cursor e ...)
  #;(dynamic-wind begin-busy-cursor
                  (lambda () e ...)
                  end-busy-cursor)
  (begin e ...))

(define (make-gui-text-world@ text-size)
  (unit
    (import)
    (export text-world^)
    
    (define the-font
      (send the-font-list
            find-or-create-font
            text-size
            'modern 'normal 'normal))
    
    (define char-bitmap-dc%
      (class bitmap-dc%
        (init-field char-width char-height)
        
        (super-new)
        
        (inherit set-text-foreground set-text-background draw-text draw-rectangle set-brush)
        
        (set-brush "black" 'solid)
        
        (define/public (draw-string s ci ri fcol bcol)
          (define x (* ci char-width))
          (define y (* ri char-height))
          (set-text-foreground fcol)
          (set-text-background bcol)
          (draw-rectangle x y (* (string-length s) char-width) char-height)
          (draw-text s x y))))
    
    (define g:draw-string (generic char-bitmap-dc% draw-string))
    
    (define scene? (is-a?/c char-bitmap-dc%))
    (define (place-image! img x y s)
      (send-generic 
       s g:draw-string                    
       (an-image-str img)
       x y 
       (an-image-fcolor img) (an-image-bcolor img)))
    
    (define (place-image*! str col row fcolor bcolor s)
      (send-generic
       s g:draw-string
       str col row fcolor bcolor))
    
    (define-struct an-image (str fcolor bcolor) #:transparent)
    (define image? an-image?)
    (define (make-image c fc bc)
      (make-an-image (string c) fc bc))
    
    (define world-canvas%
      (class canvas%
        (init-field world on-key on-draw stop-when stop! timer stop-timer timer-rate)
        (init width height)
        
        (super-new)
        
        (define dc (send this get-dc))
        (send dc set-font the-font)
        (define-values (char-width char-height char-base char-extra-vert) (send dc get-text-extent "@"))
        (define new-width (inexact->exact (* width char-width)))
        (define new-height (inexact->exact (* height char-height)))
        (send (send this get-parent) min-width new-width)
        (send (send this get-parent) min-height new-height)
        
        (define bm (make-object bitmap% new-width new-height))
        (define bm-dc (new char-bitmap-dc%
                           [bitmap bm] 
                           [char-width char-width]
                           [char-height char-height]))
        (send bm-dc set-font the-font)
        (send bm-dc set-background (send the-color-database find-color "black"))
        (send bm-dc clear)
        
        (define/public (update-world new-world)
          (cond
            [(equal? world new-world)
             (void)]
            [(stop-when new-world)
             (stop!)]
            [else
             (local [(define pre-timer (stop-timer world))
                     (define post-timer (stop-timer new-world))]
               (unless (eq? pre-timer post-timer)
                 (if post-timer
                     (send timer stop)
                     (send timer start timer-rate)))               
               (set! world new-world)             
               (send this refresh))]))
        
        (define/override (on-char event)
          (with-busy-cursor
           (update-world (on-key world event))))
        
        (define/override (on-paint)
          (with-busy-cursor
           (on-draw world bm-dc))
          (send dc draw-bitmap bm 0 0))))
    
    (define world-frame%
      (class frame%
        (init-field stop!)
        
        (define/augment (on-close)
          (stop!))
        
        (super-new)))
    
    (define (big-bang init 
                      #:height height
                      #:width width
                      #:on-tick the-on-tick
                      #:tick-rate tick-rate
                      #:on-key the-on-key
                      #:on-draw the-on-draw
                      #:stop-when the-stop-when
                      #:stop-timer the-stop-timer)      
      
      (define (stop!)
        (send timer stop)
        (send canvas enable #f)
        (send frame enable #f)
        (send frame show #f))
      
      (define frame 
        (new world-frame% 
             [label ""]
             [stop! stop!]
             [min-width (* width text-size)]
             [min-height (* height text-size)]
             [stretchable-width #f]
             [stretchable-height #f]
             [style '(no-resize-border metal)]))
      
      (define timer-rate
        (ceiling (* 1000 tick-rate)))
      (define timer
        (new timer%
             [notify-callback
              (lambda ()
                (with-busy-cursor
                 (send canvas update-world (the-on-tick (get-field world canvas)))))]
             [interval timer-rate]))
      
      (define canvas
        (new world-canvas% 
             [world init]
             [on-key the-on-key]
             [on-draw the-on-draw]
             [stop-when the-stop-when]
             [stop-timer the-stop-timer]
             [timer timer]
             [timer-rate timer-rate]
             [stop! stop!]
             [parent frame]
             [width width]
             [height height]
             [style '(no-autoclear)]))
      
      (send canvas focus)
      (send frame center)
      (send frame show #t)
      
      (call-with-exception-handler
       (lambda (x)
         ((error-display-handler) (exn-message x) x))
       (lambda ()      
         (yield 'wait))))))

(provide/contract
 [make-gui-text-world@ (exact-positive-integer? . -> . (unit/c (import) (export text-world^)))])
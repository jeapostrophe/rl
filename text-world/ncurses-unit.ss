#lang scheme
(require (only-in htdp/image
                  make-color))
(require (planet jaymccarthy/matrix)
         "../exp/ncurses/ncurses.ss"
         "../ui/ansi-colors.ss"
         "text-world-sig.ss")

; http://www.termsys.demon.co.uk/vtansi.htm 
; http://www.connectrf.com/Documents/vt220.html
; http://ascii-table.com/ansi-escape-sequences.php
(define (char->key-event c)
  (case c
    [(#\u0003) 'up]
    [(#\u0002) 'down]
    [(#\u0005) 'right]
    [(#\u0004) 'left]
    [else 
     (if (not (char-iso-control? c))
         c
         (error 'char->key-event "Can't translate ~S~n" c))]))

(define-unit ncurses-text-world@
  (import)
  (export text-world^)
  
  (define-struct scene (chars colors))
  (define (empty-scene width height)
    (make-scene
     (build-matrix height width (lambda (ri ci) #\space))
     (build-matrix height width (lambda (ri ci) ansi-reset))))
  
  (define-struct image (str color))
  (define (image-height img)
    1)
  (define (image-width img)
    (string-length (image-str img)))
  
  (define (place-image img ci ri s)
    (define str (image-str img))
    (define col (image-color img))
    (for/fold ([s s])
      ([char (in-string str)]
       [cd (in-naturals)])
      (make-scene
       (matrix-set (scene-chars s) ri (+ ci cd) char)
       (matrix-set (scene-colors s) ri (+ ci cd) col))))
  
  (define (text str color)
    (make-image str (ansi-ize color)))
  
  (define (update-display olds news do-not-update?)
    (define old (scene-chars olds))
    (define new (scene-chars news))
    (define new-cols (scene-colors news))
    (for ([ri (in-range 0 (matrix-rows new))])
      (for ([ci (in-range 0 (matrix-cols new))])
        (let ([new-char (matrix-ref new ri ci)]
              [old-char (matrix-ref old ri ci)])
          (unless (do-not-update? new-char old-char)
            (move! ri ci)
            ; XXX colors
            (add-char! new-char))))))
  
  (define (big-bang init 
                    #:on-key on-key
                    #:on-draw on-draw
                    #:stop-when stop-when)
    
    (initialize-screen!)
    (with-handlers ([exn:fail? (lambda (x)
                            (restore-screen!)
                            (raise x))])
      ; XXX use (has-colors?)
      (raw!)
      (no-newline-input!)
      (no-echo!)
      (set-keypad! stdscr #t)
      (set-interrupt-flush! stdscr #f)
      (set-meta! stdscr #t)
      (start-color!)
      
      (let ([disp (on-draw init)])
        (erase!) (refresh!)
        (update-display disp disp (lambda (new old) #f))
        (refresh!)
        (let loop ([old-disp disp] [w init])
          (if (stop-when w)
              w
              (let ([new-disp (on-draw w)])
                ; XXX Why doesn't the updating work?
                (update-display old-disp new-disp #;(lambda (new old) #f) char=?)
                (refresh!)
                (loop new-disp (on-key w (char->key-event (get-char!))))))))
      (restore-screen!))))

(provide ncurses-text-world@)
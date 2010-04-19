#lang scheme
(require scheme/system
         (only-in htdp/image
                  make-color))
(require (planet wmfarr/simple-matrix/matrix-base)
         "../ui/ansi-colors.ss"
         "text-world-sig.ss")

(define (call-with-stty thunk)
  (define stty-exe
    (or (find-executable-path "stty")
        (error 'stty "could not find executable")))
  (define (stty . args)
    (or (apply system* stty-exe args)
        (error 'stty "couldn't run with ~e" args)))
  (define (get)
    (let ([o (open-output-string)])
      (parameterize ([current-output-port o])
        (stty "-g")
        (regexp-replace #rx"\r?\n$" (get-output-string o) ""))))
  (let ([settings (get)])
    (dynamic-wind (lambda () (stty "raw" "-echo" "opost"))
                  thunk
                  (lambda () (stty settings)))))

; http://www.termsys.demon.co.uk/vtansi.htm 
; http://www.connectrf.com/Documents/vt220.html
; http://ascii-table.com/ansi-escape-sequences.php
(define (char->key-event c)
  (case c
    [(#\u001B) 
     (let ([nc (read-char)])
       (case nc
         [(#\[)
          (match (string c nc (read-char))
            ["\e[A" 'up]
            ["\e[B" 'down]
            ["\e[C" 'right]
            ["\e[D" 'left]
            [s
             (error 'char->key-event "Can't translate ~S~n" s)])]
         [else
          (error 'char->key-event "Can't translate ~S~n" nc)]))]
    [else 
     (if (not (char-iso-control? c))
         c
         (error 'char->key-event "Can't translate ~S~n" c))]))

(define-unit term-text-world@
  (import)
  (export text-world^)
  
  (define-struct scene (entries))
  (define-struct entry (char color))
  (define (empty-scene width height)
    (make-scene
     (for/matrix height width ([i (in-range (* height width))]) 
                 (make-entry #\space ansi-reset))))
     
  (define (place-image! e ci ri s)
    (matrix-set! (scene-entries s)
                 ri ci e))
  
  (define (image? x) (entry? x))
  (define (make-image c fcolor bcolor)
    ; XXX bcolor
    (make-entry c (ansi-ize fcolor)))
  
  (define (initialize-display m)
    (display "#\u001B[2J") ; Clear screen
    (update-display m))
  
  (define (update-display news)
    (for ([(ri ci e) (in-matrix (scene-entries news))])
      (printf "#\u001B[~am" (+ 30 (entry-color e))) ; Colorize
      (printf "#\u001B[~a;~af" ri (add1 ci)) ; Move cursor
      (display (entry-char e)))
    (flush-output))
  
  (define (big-bang init 
                    #:height height
                    #:width width
                    #:on-tick on-tick
                    #:tick-rate tick-rate
                    #:on-key on-key
                    #:on-draw on-draw
                    #:stop-when stop-when)
    (call-with-stty
     (lambda ()
       (define read-ch (make-channel))
       (define read-th
         (thread
          (lambda ()
            (let loop ()
              (channel-put read-ch (char->key-event (read-char)))
              (loop)))))       
       (define sc (empty-scene width height))
       (define tick-inc (* 1000 tick-rate))
       
       (on-draw init sc)       
       (initialize-display sc)
       (let loop ([old-w init] 
                  [w init] 
                  [next-tick (+ (current-inexact-milliseconds) 
                                tick-inc)])
         (if (stop-when w)
             w
             (begin
               (when (not (equal? old-w w))
                 (on-draw w sc)
                 (update-display sc))
               (sync
                (handle-evt
                 read-ch
                 (lambda (key)
                   (loop w (on-key w key) next-tick)))
                (handle-evt
                 (alarm-evt next-tick)
                 (lambda _
                   (loop w (on-tick w) (+ next-tick tick-inc))))))))))))

(provide term-text-world@)
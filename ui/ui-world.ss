#lang scheme
(require "../lib/posn.ss"
         "../lib/tree.ss"
         "../obj-world/obj-world.ss"
         "../copy-obj/this-class.ss"
         "../engine/action.ss"
         "../engine/game.ss"
         "../ui/ansi-colors.ss"         
         "../ui/keys.ss")

;; User interface
(define message-ui%
  (class%* world% (world<%>)
    (init-slot msg k)
    
    ; Keep ticking
    (define/override (on-tick) 
      (define new-k (send k on-tick))
      (if (equal? new-k k)
          this
          (copy [k new-k])))
    (define/override (on-key ke)
      ; Return the other ui when done
      (if (true-key? ke)
          (send k on-key ke)
          this))
    ; Overlay the message
    (define/override (on-draw sc)
      (send k on-draw sc)
      (place-image*! (make-string (send k width) #\space) 0 0 ansi-white ansi-black sc)
      (place-image*! msg 0 0 ansi-white ansi-black sc))
    
    (super-new)))

(define direction-ui%
  (class%* world% (world<%>)
    (init-slot k inner)
    
    (define/override (on-key ke)
      (define a (key->action ke))
      (case a
        [(up down left right left-up right-up left-down right-down)
         (k a)]
        [(ignore)
         this]
        [else
         (new message-ui%
              [msg "Please input a direction."]
              [k this])]))
    (define/override (on-draw sc)
      (send inner on-draw sc))
    
    (super-new)))

(define explore-ui%
  (class%* world% (world<%>)
    (init-slot game [waiting? #f] [done? #f])
    
    (init-slot 
     [all-posns
      (for*/fold ([t tree-empty])
        ([ri (in-range 0 (send (get-field map game) max-row))]
         [ci (in-range 0 (send (get-field map game) max-col))])
        (tree-union t (tree-node (make-posn ri ci))))])
    
    (init-slot [updated-posns 
                all-posns])
    
    (define/override (stop-when)
      done?)
    
    (define/override (stop-timer)
      waiting?)
    
    (define/override (on-tick)
      (define c (send game get-char))
      (define pre (send c get-posn))
      (match (send game tick)
        [#f
         (copy [updated-posns tree-empty]
               [waiting? #t])]
        [(list-rest changed-tree new-game)
         (define nc (send new-game get-char))
         (define post (send nc get-posn))
         (define new-this (copy 
                           [updated-posns changed-tree]
                           [game new-game]))
         (if (equal? pre post)
             new-this
             (match (send nc on-tile post)
               [(list) 
                new-this]
               [(list-rest obj (list))
                (new message-ui%
                     [msg (format "There is a ~a here." (send obj short-desc))]
                     [k new-this])]
               [_
                (new message-ui%
                     [msg "There are several things here."]
                     [k new-this])]))]))
    
    (define/override (on-key key)
      (define c (send game get-char))
      (define a (key->action key))
      (cond
        [(symbol=? a 'ignore)
         this]
        [(send c waiting-for-action?)
         (case a
           [(unknown)
            (new message-ui%
                 [msg "Unknown command."]
                 [k this])]
           [(quit)
            (copy [done? #t])]
           [(ignore) 
            this]
           [(inventory)
            (new inventory-ui%
                 [objs (send c inventory-list)]
                 [k
                  (copy [waiting? #f]
                        [updated-posns all-posns])])]
           [(open)
            (new message-ui%
                 [msg "Please input a direction."]
                 [k
                  (new direction-ui%
                       [inner this]
                       [k (lambda (dir)
                            (copy
                             [waiting? #f]
                             [updated-posns tree-empty]
                             [game
                              (send game update-char
                                    (send c input-action (make-open dir)))]))])])]
           [(close)
            (new message-ui%
                 [msg "Please input a direction."]
                 [k
                  (new direction-ui%
                       [inner this]
                       [k (lambda (dir)
                            (copy
                             [waiting? #f]
                             [updated-posns tree-empty]
                             [game
                              (send game update-char
                                    (send c input-action (make-close dir)))]))])])]
           [(throw)
            (local [(define (k obj)
                      (new message-ui%
                           [msg "Please input a direction."]
                           [k
                            (new direction-ui%
                                 [inner this]
                                 [k (lambda (dir)
                                      (copy
                                       [waiting? #f]
                                       [updated-posns tree-empty]
                                       [game
                                        (send game update-char
                                              (send c input-action (make-throw obj dir)))]))])]))]
              (match (send c inventory-list)
                [(list)
                 (new message-ui%
                      [msg "You have nothing to throw."]
                      [k this])]
                [(list-rest drop (list))
                 (k drop)]
                [objs
                 (new select-obj-ui% [objs objs] [k k] [inner this])]))]
           [(pickup)
            (local [(define cp (send c get-posn))
                    (define (k obj)
                      (new message-ui%
                           [msg (format "You picked up a ~a" (send obj short-desc))]
                           [k 
                            (copy 
                             [waiting? #f]
                             [updated-posns tree-empty]
                             [game
                              (send game update-char
                                    (send c input-action (make-pickup obj)))])]))]
              (match (send c on-tile cp)
                [(list)
                 (new message-ui%
                      [msg "There is nothing there."]
                      [k this])]
                [(list-rest pickup (list))
                 (k pickup)]
                [objs
                 (new select-obj-ui% [objs objs] [k k] [inner this])]))]
           [(drop)
            (local [(define (k obj)
                      (new message-ui%
                           [msg (format "You dropped a ~a" (send obj short-desc))]
                           [k (copy 
                               [waiting? #f]
                               [updated-posns tree-empty]
                               [game
                                (send game update-char
                                      (send c input-action (make-drop obj)))])]))]
              (match (send c inventory-list)
                [(list)
                 (new message-ui%
                      [msg "You have nothing to drop."]
                      [k this])]
                [(list-rest drop (list))
                 (k drop)]
                [objs
                 (new select-obj-ui% [objs objs] [k k] [inner this])]))]
           [(up down left right left-up right-up left-down right-down)
            (copy 
             [waiting? #f]
             [updated-posns tree-empty]
             [game
              (send game update-char
                    (send c input-action (make-move a)))])])]
        [else
         this]))
    
    (super-new)))

;; Object selection
(define select-chars (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define (select-char? c) (and (member c select-chars) #t))
(define (select-char->ref c)
  (let/ec esc
    (for ([i (in-naturals)]
          [sc (in-list select-chars)])
      (when (char=? sc c)
        (esc i)))
    #f))

(define inventory-ui%
  (class* world% (world<%>)
    (init-field objs k)
    
    (define descs
      (map (lambda (o) (send o short-desc))
           objs))
    
    (define/override (on-key ke)
      (if (true-key? ke)
          (send k on-key ke)
          this))
    
    (define/override (on-draw sc)
      (send k on-draw sc)
      (for ([d (in-list descs)]
            [c (in-list select-chars)]
            [ri (in-naturals)])
        (place-image*! (make-string (send k width) #\space) 0 (add1 ri) ansi-white ansi-black sc)
        (place-image*! (format " ~a - ~a     " c d) 0 (add1 ri) ansi-white ansi-black sc)))
    
    (super-new)))

(define select-obj-ui%
  (class* world% (world<%>)
    (init-field objs k inner)
    
    (define descs
      (map (lambda (o) (send o short-desc))
           objs))
    
    (define/override (on-key ke)
      (if (true-key? ke)
          (let ([key (send ke get-key-code)])
            (if (select-char? key)
                (let ([n (select-char->ref key)])
                  (if (n . >= . (length objs))
                      (new message-ui%
                           [msg (format "Invalid item: ~a" key)]
                           [k this])
                      (k (list-ref objs n))))
                (new message-ui%
                     [msg "Unknown command."]
                     [k this])))
          this))
    
    (define/override (on-draw sc)
      ; XXX Handle when there are different pages
      ; XXX C-rpg style
      ; XXX Check everything will fit
      ; XXX Must be able to restore what used to be underneath after, even if unchanged in world
      (send inner on-draw sc)
      (for ([d (in-list descs)]
            [c (in-list select-chars)]
            [ri (in-naturals)])
        (place-image*! (make-string (send inner width) #\space) 0 (add1 ri) ansi-white ansi-black sc)
        (place-image*! (format " ~a - ~a     " c d) 0 (add1 ri) ansi-white ansi-black sc)))
    
    (super-new)))

;; Graphics
(require "../text-world/text-world-sig.ss"
         "../text-world/cache-unit.ss"
         ; XXX Fix ncurses
         #;"../text-world/ncurses-unit.ss"
         "../text-world/gui-unit.ss")

(define font-size 16)
(define-unit-binding gui-text-world@ (make-gui-text-world@ font-size)
  (import) (export text-world^))

(define-unit-binding cache-world@ 
  (make-cache-text-world@ gui-text-world@)
  #;(make-cache-text-world@ ncurses-text-world@)  
  (import) (export text-world^))
(define-values/invoke-unit/infer cache-world@)

;;; Glyphs
(define (ascii c #:color [color ansi-white])
  (make-image c color ansi-black))

(define renderings
  (make-immutable-hasheq
   (list* 
    (cons 'visited-tunnel (ascii #\▒ #:color ansi-gray))
    (cons 'visible-tunnel (ascii #\▒ #:color ansi-white))
    (cons 'visited-floor (ascii #\. #:color ansi-gray))
    (cons 'visible-floor (ascii #\. #:color ansi-white))
    (cons 'visible-character (ascii #\@ #:color ansi-blue))
    (cons 'visited-pythonista (ascii #\P #:color ansi-dark-green))
    (cons 'visible-pythonista (ascii #\P #:color ansi-green))
    (cons 'empty (ascii #\space))
    (cons 'visible-empty (ascii #\space))
    (cons 'visited-empty (ascii #\space))
    
    (cons 'visible-wall (ascii #\| #:color ansi-yellow))
    (cons 'visited-wall (ascii #\| #:color ansi-dark-gray))
    
    (append
     (local
       [(define visible-wall ansi-yellow)
        (define visited-wall ansi-dark-gray)]
       (list* 
        (cons 'visible-closed-doorh (ascii #\╌ #:color ansi-dark-yellow))
        (cons 'visible-closed-doorv (ascii #\╎ #:color ansi-dark-yellow))
        (cons 'visible-open-doorv (ascii #\┄ #:color ansi-dark-yellow))
        (cons 'visible-open-doorh (ascii #\┆ #:color ansi-dark-yellow))
        
        (cons 'visited-closed-doorh (ascii #\╌ #:color visited-wall))
        (cons 'visited-closed-doorv (ascii #\╎ #:color visited-wall))
        (cons 'visited-open-doorh (ascii #\┄ #:color visited-wall))
        (cons 'visited-open-doorv (ascii #\┆ #:color visited-wall))
        
        (apply append
               (for*/list
                   ([above? (in-list (list #t #f))]
                    [below? (in-list (list #t #f))]
                    [left? (in-list (list #t #f))]
                    [right? (in-list (list #t #f))])
                 (define name
                   (format "~a~a~a~a-wall"
                           (if above? 'a "")
                           (if below? 'b "")
                           (if left? 'l "")
                           (if right? 'r "")))
                 (define char 
                   (match (list above? below? left? right?)
                     [(list #f #f #f #f) ; 0
                      #\o] ; Not sure (represents a pillar)
                     [(list #f #f #f #t) ; 1
                      #\├] ; Not sure
                     [(list #f #f #t #f) ; 2
                      #\┤] ; Not sure
                     [(list #f #f #t #t) ; 3
                      #\─]
                     [(list #f #t #f #f) ; 4 
                      #\┬] ; Not sure
                     [(list #f #t #f #t) ; 5
                      #\┌]
                     [(list #f #t #t #f) ; 6
                      #\┐]
                     [(list #f #t #t #t) ; 7
                      #\┬]
                     [(list #t #f #f #f) ; 8
                      #\┴] ; Not sure
                     [(list #t #f #f #t) ; 9
                      #\└]
                     [(list #t #f #t #f) ; 10
                      #\┘]
                     [(list #t #f #t #t) ; 11
                      #\┴]
                     [(list #t #t #f #f) ; 12
                      #\│]
                     [(list #t #t #f #t) ; 13
                      #\├]
                     [(list #t #t #t #f) ; 14
                      #\┤]
                     [(list #t #t #t #t) ; 15
                      #\┼]))           
                 (list
                  (cons (string->symbol (format "visible-~a" name))
                        (ascii char #:color visible-wall))
                  (cons (string->symbol (format "visited-~a" name))
                        (ascii char #:color visited-wall)))))))
     (apply 
      append
      (for/list 
          ([color-name (in-list (list 'red 'green 'yellow 'blue 'magenta 'cyan))]
           [dark-color (in-list (list ansi-dark-red ansi-dark-green ansi-dark-yellow ansi-dark-blue ansi-dark-magenta ansi-dark-cyan))]
           [light-color (in-list (list ansi-red ansi-green ansi-yellow ansi-blue ansi-magenta ansi-cyan))])
        (apply append
               (for/list ([dir (in-list (list #\( #\)))]
                          [orient (in-list (list 'left 'right))])
                 (list
                  (cons (string->symbol (format "visible-~a-~a-paren" color-name orient))
                        (ascii dir #:color light-color))
                  (cons (string->symbol (format "visited-~a-~a-paren" color-name orient))
                        (ascii dir #:color dark-color)))))))))))
(define (render-sym->image s)
  (hash-ref renderings s (lambda () (error 'render-sym->image "Unknown render sym: ~e" s))))

#;(for ([(k v) (in-hash renderings)])
    (printf "~a = ~a~n" k v))

;; Rendering
(define nethack-explore-ui%
  (class%* explore-ui% (world<%>)
    (inherit-field game updated-posns)
    
    (super-new)
    
    (define map-rows
      (send (get-field map game) max-row))
    (define map-cols
      (send (get-field map game) max-col))
    
    (define/override (height) (+ map-rows 3))
    (define/override (width) map-cols)
    
    (define (posn-y p)
      (add1 (posn-row p)))
    (define (posn-x p) (posn-col p))
    (define (render-on-posn img p s)
      (place-image! 
       img
       (posn-x p)
       (posn-y p)
       s))
    
    (define (render-msg m s)
      ; XXX use message-ui
      (place-image*! (make-string (width) #\space) 0 0 ansi-white ansi-black s)
      (place-image*! m 0 0 ansi-white ansi-black s))
    
    (define (render-status-line title snd s)
      (place-image*! (make-string (width) #\space) 0 (+ map-rows 2) ansi-white ansi-black s)
      (place-image*! snd 0 (+ map-rows 2) ansi-white ansi-black s)
      (place-image*! (make-string (width) #\space) 0 (+ map-rows 1) ansi-white ansi-black s)
      (place-image*! title 0 (+ map-rows 1) ansi-white ansi-black s))
    
    (define (render-map sc)
      (define c (send game get-char))
      (define m (get-field map game))
      (tree-for-each 
       (lambda (p)
         (render-on-posn 
          (render-sym->image 
           (send c visualize p (send m tile p)))
          p sc))
       updated-posns))
    
    (define/override (on-draw sc)
      (define c (send game get-char))
      (render-map sc)
      (render-msg (send c current-message) sc)
      (render-status-line
       (send c short-desc)
       (format "T:~a" 
               (send game game-time))
       sc))))

;; Setup
(define first-game 
  (initial-game
   #:rows 24
   #:cols 80))
(define initial-world (new nethack-explore-ui% [game first-game]))

(define RATE 1/30)
(define (big-bang/obj w)
  (define g:on-tick (generic world<%> on-tick))
  (define g:on-key (generic world<%> on-key))
  (define g:on-draw (generic world<%> on-draw))
  (define g:stop-when (generic world<%> stop-when))
  (define g:stop-timer (generic world<%> stop-timer))
  (big-bang w
            #:height (send w height)
            #:width (send w width)
            #:on-tick (lambda (w) (send-generic w g:on-tick))
            #:tick-rate RATE
            #:on-key (lambda (w key) (send-generic w g:on-key key))
            #:on-draw (lambda (w sc) (send-generic w g:on-draw sc))
            #:stop-when (lambda (w) (send-generic w g:stop-when))
            #:stop-timer (lambda (w) (send-generic w g:stop-timer))))

(define (go)
  (big-bang/obj initial-world))

; http://docs.plt-scheme.org/profile/index.html
(require profile)
(profile (go))
#;(go)

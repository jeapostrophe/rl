#lang scheme
(require "../lib/tree.ss"
         #;"fov.ss"
         "slow-fov.ss"
         "action.ss"
         "timed.ss"
         "tile.ss"
         "map.ss"
         "obj.ss"
         "action.ss"
         "missile.ss"
         "../copy-obj/this-class.ss"
         "../lib/posn.ss"
         "../lib/matrix-posn.ss")

(define mob<%>
  (interface (timed<%>)
    short-desc
    move-posn move-floor
    get-posn
    inventory-list inv-pickup inv-drop
    render
    visualize))

(define-struct tile-memory (visible? seen?))

(define mob%
  (timed-mixin
   (class%* object% (mob<%>)
     (init-slot posn)
     
     (define/public (short-desc)
       (error 'short-desc "No short description"))
     
     (define/public (get-posn) posn)
     (define/public (move-posn map moving-p)
       (define-values (changed new-this)
         (send (copy [posn moving-p]) look map))
       new-this)
     
     (init-slot [current-floor-id #f])
     (define/public (move-floor new-floor-id new-floor-map)
       (copy [current-floor-id new-floor-id]
             [floor-memory (update-floor-memory new-floor-id new-floor-map)]))
     
     (init-slot [floor-memory (make-immutable-hasheq empty)])
     (define/public (current-floor-map)
       (car (hash-ref floor-memory current-floor-id)))
     (define (current-floor-mem)
       (cdr (hash-ref floor-memory current-floor-id)))
     (define (current-tile-mem p)
       (matrix-ref/posn (current-floor-mem) p))
     (define (update-floor-memory id map)
       (define map-rows (send map max-row))
       (define map-cols (send map max-col))
       (unless (hash-ref floor-memory id #f)
         (hash-set floor-memory id
                   (cons (empty-map map-rows map-cols)
                         (build-matrix map-rows map-cols (lambda (r c) (make-tile-memory #f #f)))))))
     
     (init-slot [visible empty])
     (define/public (look new-map)
       (define newly-visible
         (update-fov 
          (send new-map max-row)
          (send new-map max-col)                     
          (lambda (p)
            (send new-map valid-tile? p))
          (lambda (p)
            (send (send new-map tile p) obstruction?))
          posn visual-radius))
       (define new-floor-memory
         (hash-update 
          floor-memory current-floor-id
          (match-lambda
            [(cons old-map old-tile-mem)
             (define map-rows
               (matrix-rows old-tile-mem))
             (define map-cols
               (matrix-cols old-tile-mem))
             (define reset-tile-mem
               (for/fold ([m old-tile-mem])
                 ([old-visible-p visible])
                 (matrix-set/posn m old-visible-p (make-tile-memory #f #t))))
             (define-values
               (next-map next-tile-mem)
               (for/fold ([map old-map]
                          [tile-mem reset-tile-mem])
                 ([visible-p newly-visible])
                 (values (send map update-tile visible-p
                               (send new-map tile visible-p))
                         (matrix-set/posn tile-mem visible-p
                                          (make-tile-memory #t #t)))))
             (cons next-map next-tile-mem)])))
       (values
        (tree-union (tree-list visible) (tree-list newly-visible))
        (copy
         [visible newly-visible]
         [floor-memory
          new-floor-memory])))
     
     (define/public (can-see? p)
       (tile-memory-visible? (current-tile-mem p)))
     (define/public (has-seen? p)
       (tile-memory-seen? (current-tile-mem p)))
     (define/public (on-tile p)
       (send (send (current-floor-map) tile p) on-tile))
     (define/public (in-tile p)
       (send (send (current-floor-map) tile p) in-tile))
     
     (init-slot [visual-radius 5])       
     
     (init-slot [inv empty])
     (define/public (inventory-list)
       inv)
     (define/public (inv-pickup obj)
       (copy [inv (append inv (list obj))]))
     (define/public (inv-drop obj)
       (copy [inv (remq obj inv)]))
     
     (define/public (render)
       'character)
     (define/public (visualize p o)
       (define r (send o render))
       (cond
         [(or (can-see? p)
              (has-seen? p))
          (let ([in (send this in-tile p)]
                [on (send this on-tile p)])
            (string->symbol
             (format "~a-~a"
                     (if (can-see? p)
                         "visible"
                         "visited")
                     (local
                       [(define (check-around? dr dc . r)
                          (define np (posn-adjust p dr dc))
                          (and (send (current-floor-map) valid-tile? np)
                               (let ([c (send (send (current-floor-map) tile np) render)])
                                 (ormap (lambda (r) (symbol=? c r))
                                        r))))]
                       (cond
                         [in
                          (send in render)]
                         [(not (empty? on))
                          (send (first on) render)]
                         [(or (symbol=? r 'closed-door)
                              (symbol=? r 'open-door))
                          (local
                            [(define (wall? dr dc) (check-around? dr dc 'wall))
                             (define above? (wall? -1 0))
                             (define below? (wall? 1 0))
                             (define left? (wall? 0 -1))
                             (define right? (wall? 0 1))]
                            (cond
                              [(or above? below?)
                               (string->symbol (format "~av" r))]
                              [else
                               (string->symbol (format "~ah" r))]))]
                         [(symbol=? r 'wall)
                          (local
                            [(define (wall? dr dc) (check-around? dr dc 'wall 'closed-door 'open-door))
                             (define above? (wall? -1 0))
                             (define below? (wall? 1 0))
                             (define left? (wall? 0 -1))
                             (define right? (wall? 0 1))]
                            (if (or above? below? left? right?)
                                (string->symbol
                                 (format "~a~a~a~a-wall"
                                         (if above? 'a "")
                                         (if below? 'b "")
                                         (if left? 'l "")
                                         (if right? 'r "")))
                                (case (posn-direction posn p)
                                  [(horiz)
                                   'ab-wall]
                                  [(vert)
                                   'lr-wall]
                                  [(dright)
                                   'al-wall]
                                  [(uleft)
                                   'br-wall]
                                  [(dleft)
                                   'ar-wall]
                                  [(uright)
                                   'bl-wall])))]
                         [else
                          r])))))]
         [else
          'empty]))
     
     (define/public (speed)
       NORMAL-SPEED)
     
     (define/public (next-action)
       (error 'next-action "No next action"))
     (define/public (activate game)
       (define-values (changed new-this)
         (send this look (get-field map game)))
       (match (send new-this next-action)
         [#f
          #f]
         [(cons new-this a)
          (define ar
            (match a
              [(struct drop (obj))
               (send new-this do-drop game obj)]
              [(struct pickup (obj))
               (send new-this do-pickup game obj)]
              [(struct throw (obj dir))
               (send new-this do-throw game obj dir)]
              [(struct open (dir))
               (send new-this do-open game dir)]
              [(struct close (dir))
               (send new-this do-close game dir)]
              ; XXX Automatically try to open door in UI
              [(struct move (dir))
               (send new-this do-move game dir)]))
          (struct-copy activate-result ar
                       [updated-posns (tree-union (activate-result-updated-posns ar) changed)])]))
     
     (define/public (do-open game dir)
       (define tp (direction->posn dir posn))
       (define map (get-field map game))
       (define tile (send map tile tp))
       (if (is-a? tile openable<%>)
           (if (send tile can-open?)
               (make-activate-result 
                NORMAL-SPEED this
                (send game update-map
                      (send map update-tile tp
                            (send tile open)))
                (tree-node tp) empty)
               (make-activate-result
                NO-SPEED (send this receive-message "You cannot open it")
                game tree-empty empty))
           (make-activate-result
            NO-SPEED (send this receive-message "That cannot be opened.")
            game tree-empty empty)))
     
     (define/public (do-close game dir)
       (define tp (direction->posn dir posn))
       (define map (get-field map game))
       (define tile (send map tile tp))
       (if (is-a? tile openable<%>)
           (if (send tile can-close?)
               (make-activate-result 
                NORMAL-SPEED this
                (send game update-map
                      (send map update-tile tp
                            (send tile close)))
                (tree-node tp) empty)
               (make-activate-result
                NO-SPEED (send this receive-message "You cannot close it")
                game tree-empty empty))
           (make-activate-result
            NO-SPEED (send this receive-message "That cannot be closed.")
            game tree-empty empty)))
     
     ; XXX Should update current tile with new this
     ; XXX A bit weird that when you throw something and it can't move one square it ends up on the floor
     ; XXX Also, objects should be "in the air" not on the ground
     (define/public (do-throw game obj dir)
       (define map (get-field map game))
       (define tile (send map tile posn))
       (make-activate-result 
        NORMAL-SPEED (send this inv-drop obj)
        (send game update-map
              (send map update-tile posn
                    (send tile drop obj)))
        (tree-node posn)
        (list (new missile% [obj obj] [dir dir] [posn posn]))))
     
     ; XXX Should update current tile with new this
     (define/public (do-pickup game obj)
       (define map (get-field map game))
       (define tile (send map tile posn))
       (make-activate-result 
        NORMAL-SPEED (send this inv-pickup obj)
        (send game update-map
              (send map update-tile posn
                    (send tile pickup obj)))
        (tree-node posn) empty))
     
     ; XXX Should update current tile with new this
     (define/public (do-drop game obj)
       (define map (get-field map game))
       (define tile (send map tile posn))
       (if (send tile can-drop?)
           (make-activate-result 
            NORMAL-SPEED (send this inv-drop obj)
            (send game update-map
                  (send map update-tile posn
                        (send tile drop obj)))
            (tree-node posn) empty)
           (make-activate-result
            NO-SPEED (send this receive-message "You cannot drop anything")
            game tree-empty empty)))
     
     (define/public (do-move game act)
       (define map (get-field map game))
       (define old-p-tile
         (send map tile posn))
       (define-values (can-leave? leave-msg) 
         (send old-p-tile leave?))
       (if can-leave?
           (let ([moving-p (direction->posn act posn)])
             (if (send map valid-tile? moving-p)
                 (local [(define moving-tile (send map tile moving-p))
                         (define-values (can-enter? enter-msg)
                           (send moving-tile enter?))]
                   (if can-enter?
                       (local [(define map1
                                 (send map update-tile posn
                                       (send old-p-tile move-away this)))
                               (define map2
                                 (send map1 update-tile moving-p
                                       (send moving-tile move this)))
                               (define new-this
                                 (send (send this move-posn map2 moving-p)
                                       receive-message enter-msg))]
                         (make-activate-result
                          NORMAL-SPEED new-this
                          (send game update-map map2)
                          (tree-union 
                           (tree-node posn)
                           (tree-node moving-p)) empty))
                       (make-activate-result 
                        NO-SPEED (send this receive-message enter-msg) game
                        tree-empty empty)))
                 (make-activate-result 
                  NO-SPEED (send this receive-message "You bonk your head on the Outer Limits") game
                  tree-empty empty)))
           (make-activate-result 
            NO-SPEED (send this receive-message leave-msg) game
            tree-empty empty)))
     
     (super-new))))

(provide/contract
 [mob<%> interface?]
 [mob% (implementation?/c mob<%>)])
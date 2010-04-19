#lang scheme
; Based on http://roguebasin.roguelikedevelopment.org/index.php?title=An_elegant_time-management_system_for_roguelikes
(require "../lib/tree.ss"
         "../copy-obj/this-class.ss")

; Standard speeds
(define speed/c number?)
(define NORMAL-SPEED 100)
(define NO-SPEED 0)

(provide/contract
 [speed/c contract?]
 [NO-SPEED speed/c]
 [NORMAL-SPEED speed/c])

; Timed objects
(define timed<%>
  (interface ()
    speed activate))

(define timer<%>
  (interface (timed<%>)
    get-energy update-energy))

(define (timed-mixin %)
  (class%* % (timer<%>)
    (super-new)
    
    (init-slot [energy (* -1 (send this speed))])
    
    (define/public (get-energy) energy)
    (define/public (update-energy new) (copy [energy new]))))

; Queue management
(require "../lib/fair-heap.ss")

(define (make-timed-objects obj)
  (fair-heap-insert (send obj get-energy) obj fair-heap-empty))

(define (register-timed-object tes obj)
  (fair-heap-insert (send obj get-energy) obj tes))

(define-struct activate-result (the-cost new-obj new-game updated-posns new-objs))
(define-struct progress-result (new-dq new-game updated-posns))

(define (insert* es h)
  (if (null? es)
      h
      (insert* 
       (rest es) 
       (register-timed-object h (first es)))))

(define (progress dq game)
  (define obj (fair-heap-min dq))
  (define energy (send obj get-energy))
  #;(printf "progress ~S ~S~n" energy obj)
  (if (energy . <= . 0)
      (match (send obj activate game)
        [#f
         #f]
        [(struct activate-result (the-cost new-obj new-game updated-posns new-objs))
         (define dq-min-obj
           (fair-heap-remove-min dq))
         (define new-dq
           (if new-obj
               (let* ([new-energy (+ energy the-cost)]
                      [new-te (send new-obj update-energy new-energy)])
                 (fair-heap-insert new-energy new-te dq-min-obj))
               dq-min-obj))
         (define next-dq 
           (insert* new-objs new-dq))         
         #;(printf "activate~n")
         (make-progress-result next-dq new-game updated-posns)])
      (local [(define dq-min-obj
                (fair-heap-remove-min dq))
              (define new-energy
                (- energy (send obj speed)))
              (define new-obj
                (send obj update-energy new-energy))]        
        #;(printf "quantum~n")
        (progress
         (fair-heap-insert new-energy new-obj dq-min-obj)
         game))))

(define timed/c fair-heap?)

(define timed-elements fair-heap-elements)

(define (timed-map f h)
  (fair-heap-map 
   (lambda (o)
     (define no (f o))
     (values (send no get-energy)
             no))
   h))

(provide/contract
 [timed<%> interface?]
 [timed-mixin ((implementation?/c timed<%>) . -> . (implementation?/c timer<%>))]
 [make-timed-objects ((is-a?/c timer<%>) . -> . timed/c)]
 [register-timed-object (timed/c (is-a?/c timer<%>) . -> . timed/c)]
 [struct activate-result ([the-cost number?]
                          [new-obj (or/c false/c (is-a?/c timer<%>))]
                          ; XXX better contract
                          [new-game object?]
                          [updated-posns tree/c]
                          [new-objs (listof (is-a?/c timer<%>))])]
 [struct progress-result ([new-dq timed/c]
                          [new-game object?]
                          [updated-posns tree/c])]
 [progress (timed/c object? . -> . (or/c progress-result? false/c))]
 [timed-elements (timed/c . -> . (listof (is-a?/c timer<%>)))]
 [timed-map (((is-a?/c timer<%>) . -> . (is-a?/c timer<%>))
             timed/c . -> . timed/c)])
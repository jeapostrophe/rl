#lang scheme
(require "../lib/batched-deque.ss"
         "../lib/random.ss"
         "../copy-obj/this-class.ss"
         "timed.ss"
         "mob.ss")

(define char<%> 
  (interface (mob<%>)
    input-action
    current-message receive-message))

(define CHARS
  `(["Matthias" "Monarch"]
    ["Shriram" "Auto-mechanic"]
    ["Matthew" "Core"]
    ["Robby" "Lawyer"]
    ["John" "Foot"]
    ["Eli" "Amazing"]
    ["Ryan" "Macrologist"]))

(define char%
  (class%* mob% (char<%>)
    (init-slot [char (random-element CHARS)])
    (define/override (short-desc)
      (format "~a the ~a" 
              (first char)
              (second char)))
    
    ; XXX remove msg
    (init-slot [msg ""])
    (define/public (current-message)
      msg)
    (define/public (receive-message m)
      (copy [msg m]))
    
    (init-slot [input deque-empty])
    (define/public (input-action a)
      (copy [input (deque-push a input)]))
    (define/override (next-action)
      (if (deque-empty? input)
          #f
          (cons (copy [input (deque-shift input)])
                (deque-first input))))
    (define/public (waiting-for-action?)
      (deque-empty? input))
    
    (define/public (score)
      0)
    
    (define/override (speed)
      (* 3/2 NORMAL-SPEED))
    
    (super-new)))

(provide/contract
 [char<%> interface?]
 [char% (implementation?/c char<%>)])

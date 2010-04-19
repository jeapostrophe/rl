#lang scheme
(require "this-class.ss")

;; Example

;; Just to make inspecting them at the repl easier
(define old-insp (current-inspector))
(current-inspector (make-inspector))

;; Examples

(define foo%
  (class% object%
          (init-slot [a 0])
          (slot [b 1])
          (define/public (a-up) (copy [a (add1 a)]))
          (define/public (set-b! new-b) (set! b new-b))
          (super-new)))

(define bar%
  (class% foo%
          (init-slot [d 3])
          (define/public (d-down) (copy [d (sub1 d)]))
          (super-new)))

(current-inspector old-insp)

(define foo (new foo%))
(define bar (new bar%))

foo
bar
(send bar set-b! 12)
bar

(send bar a-up)
(send (send bar a-up) d-down)

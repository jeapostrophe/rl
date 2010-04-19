#lang scheme
(require tests/eli-tester
         "cobj.ss")

(printf "Guide example~n")

(define fish<%>
  (interface (object<%>)
    [new (number? . -> . this?)]
    [get-size (-> number?)]
    [grow (number? . -> . this?)]
    ; XXX is-a fish<%>
    [eat (this? . -> . this?)]))

(define fish%
 (class* object% fish<%>
   (fields size)
   (define (new size)
     (make-this size))
   (define (get-size)
     size)
   (define (grow amt)
     (update-this [size (+ amt size)]))
   (define (eat other-fish)
     (send obj grow (send other-fish get-size)))))

(define charlie (new fish% 10))
(define charlie-p (send charlie grow 6))
(test
 (send charlie-p get-size) => 16)

(define hungry-fish<%>
  (interface (fish<%>)
    ; XXX is-a fish<%>
    [eat-more (this? this? . -> . this?)]))

#;(define hungry-fish%
  (class* fish% hungry-fish<%>
    (define (eat-more fish1 fish2)
      (send (send this eat fish1) eat fish2))))

#;(define harry (new hungry-fish% 10))
#;(test
 (send (send harry eat-more charlie charlie) get-size) => 30)

(printf "My example~n")

(define posn<%>
  (interface (object<%>)
    [new (number? number? . -> . this?)]
    [get-x (-> number?)]
    [get-y (-> number?)]
    [adjust (number? number? . -> . this?)]))

(define posn%
  (class* object% posn<%>
    (fields x y)
    (define (new x y)
      (make-this x y))
    (define (get-x) x)
    (define (get-y) y)
    (define (adjust dx dy)
      (update-this [x (+ x dx)]
                   [y (+ y dy)]))))

(define p0 (new posn% 0 0))
(send p0 get-x)
(send p0 get-y)

(define p1 (send p0 adjust 1 0))
(send p1 get-x)
(send p1 get-y)

(define p2 (send/apply p0 adjust (list 1 0)))
(send p2 get-x)
(send p2 get-y)

#;(new posn% 'x 0)
#;(send p0 adjust 'x 0)

(define colored-posn<%>
  (interface (posn<%>)
    [new (number? number? symbol? . -> . this?)]
    [get-color (-> symbol?)]))

(define colored-posn%
  (class* object% colored-posn<%>
    (fields super color)
    (define (new x y color)
      (make-this (new posn% x y) color))
    (define (get-x) (send super get-x))
    (define (get-y) (send super get-y))
    (define (get-color) color)
    (define (adjust dx dy)
      (update-this 
       [super
        (send super adjust dx dy)]))))

#;(define colored-posn%
    (class* posn% colored-posn<%>
      (fields color)
      (define (new x y color)
        (make-this (new super% x y) color))
      (define-super get-x)
      (define-super/update adjust)
      (define (get-y) (send super get-y))
      (define (get-color) color)))

(define cp1 (new colored-posn% 1 2 'red))
(send cp1 get-x)
(send cp1 get-y)
(send cp1 get-color)
(define cp2 (send cp1 adjust 1 2))
(send cp2 get-x)
(send cp2 get-y)
(send cp2 get-color)

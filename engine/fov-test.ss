#lang scheme
; This shows how the two fov algorithms are different
(require "../lib/posn.ss"
         "level-gen.ss"
         (prefix-in fast: "fov.ss")
         (prefix-in slow: "slow-fov.ss")
         tests/eli-tester)

(define max-rows 24)
(define max-cols 80)
(define posn-valid? (make-valid-posn? max-rows max-cols))
(define radius 5)

(define (sort-posns ps)
  (sort ps posn<=))

(define (compare)
  (define map (generate-map max-rows max-cols))
  (define (obstruction? p)
    (send (send map tile p) obstruction?))
  (define origin
    (send map random-posn (lambda (p) (not (send (send map tile p) obstruction?)))))
  (define fast-version
    (sort-posns
     (fast:update-fov max-rows max-cols posn-valid? obstruction? origin radius)))
  (define slow-version
    (sort-posns
     (slow:update-fov max-rows max-cols posn-valid? obstruction? origin radius)))
  (printf "Origin: ~S~n" origin)
  (printf "Fast: ~S~n" fast-version)
  (printf "Slow: ~S~n" slow-version)
  (test
   fast-version => slow-version))

(compare)
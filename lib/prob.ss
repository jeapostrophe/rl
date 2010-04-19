#lang scheme
(require (only-in srfi/27 random-real))

(define with-probability*
  (match-lambda
    [(and probs (list (cons prob body-thnk) ...))
     (define sum (apply + prob))
     (unless (= sum 1)
       (error 'with-probability* "Total probability is not 1: ~a" sum))
     (with-probability-select (random-real) probs)]))
(define (with-probability-select v probs)
  (match probs
    [(list)
     (error 'with-probability-select "Strange probability: ~a" v)]
    [(list-rest (cons prob body-thnk) probs)
     (if (< v prob)
         (body-thnk)
         (with-probability-select (- v prob) probs))]))  
(define-syntax-rule (with-probability [prob expr ...] ...)
  (with-probability* (list (cons prob (lambda () expr ...)) ...)))

(provide with-probability)
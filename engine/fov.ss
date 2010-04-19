#lang scheme
#|
This doesn't do the same thing as slow-fov, so I trust it less.

Based on
 http://www.geocities.com/temerra/los_rays.html
 http://www.visualharmonics.co.uk/actionscript-3/as3-conversion-of-modelling-rays-for-line-of-sight-in-an-object-rich-world/
|#
(require (planet jaymccarthy/matrix)
         "../lib/posn.ss")
(require (planet cce/scheme:4:1/queue))

(define-struct ray-data 
  (x-loc y-loc 
         x-obsc y-obsc
         x-err-obsc y-err-obsc
         x-input y-input
         added? ignore? obstruction?)
  #:mutable)
(define (build-ray-data xl yl)
  (make-ray-data 
   xl yl
   0 0
   0 0
   #f #f
   #f #f
   #f))
(define ray-data-x-obscured?
  (match-lambda
    [(struct ray-data
             (x-loc y-loc x-obsc y-obsc x-err-obsc y-err-obsc x-input y-input added? ignore? obstruction?))
     (and (> x-err-obsc 0) (<= x-err-obsc x-obsc))]))
(define ray-data-y-obscured?
  (match-lambda
    [(struct ray-data
             (x-loc y-loc x-obsc y-obsc x-err-obsc y-err-obsc x-input y-input added? ignore? obstruction?))
     (and (> y-err-obsc 0) (<= y-err-obsc y-obsc))]))
(define (ray-data-obscured? rd)
  (or (ray-data-x-obscured? rd)
      (ray-data-y-obscured? rd)))
(define ray-data-x-recessive?
  (match-lambda
    [(struct ray-data
             (x-loc y-loc x-obsc y-obsc x-err-obsc y-err-obsc x-input y-input added? ignore? obstruction?))
     (and (<= x-err-obsc 0) (> x-obsc 0))]))
(define ray-data-y-recessive?
  (match-lambda
    [(struct ray-data
             (x-loc y-loc x-obsc y-obsc x-err-obsc y-err-obsc x-input y-input added? ignore? obstruction?))
     (and (<= y-err-obsc 0) (> y-obsc 0))]))
(define (ray-data-recessive? rd)
  (or (ray-data-x-recessive? rd)
      (ray-data-y-recessive? rd)))

(define ((make-process-input! ray-data-x-input
                              ray-data-x-err-obsc ray-data-x-obsc
                              set-ray-data-x-err-obsc! ray-data-y-obsc
                              set-ray-data-y-err-obsc! ray-data-y-err-obsc
                              set-ray-data-y-obsc!
                              set-ray-data-x-obsc!)
         rd)
  (define xi (ray-data-x-input rd))
  (when (and (> (ray-data-x-err-obsc xi) 0)
             (zero? (ray-data-x-obsc rd)))
    (set-ray-data-x-err-obsc! 
     rd (- (ray-data-x-err-obsc xi)
           (ray-data-y-obsc xi)))
    (set-ray-data-y-err-obsc! 
     rd (+ (ray-data-y-err-obsc xi)
           (ray-data-y-obsc xi)))
    (set-ray-data-y-obsc! rd (ray-data-y-obsc xi))
    (set-ray-data-x-obsc! rd (ray-data-x-obsc xi)))
  (when (and (<= (ray-data-y-err-obsc xi) 0)
             (> (ray-data-y-obsc xi) 0)
             (> (ray-data-x-err-obsc xi) 0))
    (set-ray-data-y-err-obsc! 
     rd (+ (ray-data-y-err-obsc xi)
           (ray-data-y-obsc xi)))
    (set-ray-data-x-err-obsc! 
     rd (- (ray-data-x-err-obsc xi)
           (ray-data-y-obsc xi)))
    (set-ray-data-y-obsc! rd (ray-data-y-obsc xi))
    (set-ray-data-x-obsc! rd (ray-data-x-obsc xi))))

(define process-x-input!
  (make-process-input! ray-data-x-input
                       ray-data-x-err-obsc ray-data-x-obsc
                       set-ray-data-x-err-obsc! ray-data-y-obsc
                       set-ray-data-y-err-obsc! ray-data-y-err-obsc
                       set-ray-data-y-obsc!
                       set-ray-data-x-obsc!))

(define process-y-input!
  (make-process-input! ray-data-y-input
                       ray-data-y-err-obsc ray-data-y-obsc
                       set-ray-data-y-err-obsc! ray-data-x-obsc
                       set-ray-data-x-err-obsc! ray-data-x-err-obsc
                       set-ray-data-y-obsc!
                       set-ray-data-x-obsc!))

(define (merge-inputs! obstruction? rd)
  (define xl (ray-data-x-loc rd))
  (define yl (ray-data-y-loc rd))
  (define xi (ray-data-x-input rd))
  (define yi (ray-data-y-input rd))
  (when xi
    (process-x-input! rd))
  (when yi
    (process-y-input! rd))
  (cond
    [(or (and (not xi) (not yi))
         (and (not xi) (ray-data-obscured? yi))
         (and (not yi) (ray-data-obscured? xi)))
     (set-ray-data-ignore?! rd #t)]
    [(and (not (ray-data-obscured? rd))
          (obstruction? rd))
     (set-ray-data-obstruction?! rd #t)
     (set-ray-data-x-obsc! rd (abs xl))
     (set-ray-data-y-obsc! rd (abs yl))
     (set-ray-data-x-err-obsc! rd (abs xl))
     (set-ray-data-y-err-obsc! rd (abs yl))]))

(define (update-fov max-rows max-cols posn-valid? obstruction? origin radius)
  (define visual
    (build-matrix 
     max-rows max-cols
     (lambda (r c) #f)))
  (define perimeter
    (make-queue))  
  (define (valid? p)
    (and (posn-valid? p)
         ((abs (posn-distance origin p)) . <= . radius)))  
  (define (ray-data-posn rd)
    (posn-adjust origin (ray-data-y-loc rd) (ray-data-x-loc rd)))
  (define (expand-perimeter-from! from)
    (define xl (ray-data-x-loc from))
    (define yl (ray-data-y-loc from))
    (when (xl . >= . 0)
      (process-ray! (build-ray-data (add1 xl) yl) from))
    (when (xl . <= . 0)
      (process-ray! (build-ray-data (sub1 xl) yl) from))
    (when (yl . >= . 0)
      (process-ray! (build-ray-data xl (add1 yl)) from))
    (when (yl . <= . 0)
      (process-ray! (build-ray-data xl (sub1 yl)) from)))
  (define (process-ray! new input)
    (define map-posn (ray-data-posn new))
    (when (valid? map-posn)
      (let ([new
             (cond
               [(matrix-ref visual (posn-row map-posn) (posn-col map-posn))
                => (lambda (new) new)]
               [else new])])
        (if (= (ray-data-y-loc new)
               (ray-data-y-loc input))
            (set-ray-data-x-input! new input)
            (set-ray-data-y-input! new input))
        (unless (ray-data-added? new)
          (enqueue! perimeter new)
          (set-ray-data-added?! new #t)
          (matrix-set! visual (posn-row map-posn) (posn-col map-posn) new)))))
  (define (cast-rays!)
    (unless (queue-empty? perimeter)
      (local [(define cd (dequeue! perimeter))]
        (merge-inputs! (lambda (rd) (obstruction? (ray-data-posn rd))) cd)
        (unless (ray-data-ignore? cd)
          (expand-perimeter-from! cd))
        (cast-rays!))))
  
  (expand-perimeter-from! (build-ray-data 0 0))
  (cast-rays!)
  
  (list* 
   origin
   (for*/fold ([viewable empty]) 
     ([ri (in-range 0 max-rows)]
      [ci (in-range 0 max-cols)])
     (let ([p (make-posn ri ci)]
          [rd (matrix-ref visual ri ci)])
       (if (or (not (ray-data? rd))
               (ray-data-ignore? rd)
               (and (not (ray-data-obstruction? rd))
                    (ray-data-obscured? rd))
               ; Not sure what recessive? means
               (ray-data-recessive? rd))
           viewable
           (list* p viewable))))))

(provide/contract
 [update-fov (exact-positive-integer?
              exact-positive-integer?
              (posn? . -> . boolean?)
              (posn? . -> . boolean?)
              posn?
              exact-nonnegative-integer?
              . -> .
              (listof posn?))])
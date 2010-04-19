#lang scheme/gui

; http://en.wikipedia.org/wiki/ANSI_escape_code
(define-syntax (define-ansi-colors stx)
  (syntax-case stx ()
    [(_ (code id color) ...)
     (with-syntax
         ([ansi-colors
           (datum->syntax stx 'ansi-colors stx)]
          [(ansi-code ...)
           (map (lambda (i) (datum->syntax stx (string->symbol (format "ansi-~a" (syntax->datum i))) stx))
                (syntax->list #'(id ...)))])
       (syntax/loc stx
         (begin
           (define ansi-colors
             (list (cons code color) ...))
           (define ansi-code color)
           ...
           (provide/contract
            [ansi-code (is-a?/c color%)]
            ...))))]))

(define (make-color r g b)
  (make-object color% r g b))
(define (color-red c)
  (send c red))
(define (color-green c)
  (send c green))
(define (color-blue c)
  (send c blue))

(define-ansi-colors
  (0 black (make-color 0 0 0))
  (1 dark-red (make-color 128 0 0))
  (2 dark-green (make-color 0 128 0))
  (3 dark-yellow (make-color 128 128 0))
  (4 dark-blue (make-color 0 0 128))
  (5 dark-magenta (make-color 128 0 128))
  (6 dark-cyan (make-color 0 128 128))
  (7 dark-gray (make-color 192 192 192))
  
  (60 gray (make-color 128 128 128))
  (61 red (make-color 255 0 0))
  (62 green (make-color 0 255 0))
  (63 yellow (make-color 255 255 0))
  (64 blue (make-color 0 0 255))
  (65 magenta (make-color 255 0 255))
  (66 cyan (make-color 0 255 255))
  (67 white (make-color 255 255 255)))
(define ansi-reset 9)

(define (color-distance c1 c2)
  (+ (sqr (- (color-red c1) (color-red c2)))
     (sqr (- (color-green c1) (color-green c2)))
     (sqr (- (color-blue c1) (color-blue c2)))))

(define (color-closer color estimate1 estimate2)
  (if (<= (color-distance color (cdr estimate1))
          (color-distance color (cdr estimate2)))
      estimate1
      estimate2))

(define (color-closest color colors)
  (for/fold ([closest-so-far (first colors)])
    ([next-estimate (rest colors)])
    (color-closer color closest-so-far next-estimate)))

(define (coerce-color c)
  (cond
    [(string? c)
     (send the-color-database find-color c)]
    [else
     c]))

(define (ansi-ize c)
  (car (color-closest (coerce-color c) ansi-colors)))

(provide/contract
 [ansi-ize ((or/c string? (is-a?/c color%)) . -> . exact-nonnegative-integer?)]
 [ansi-reset exact-nonnegative-integer?])
#lang scheme
(require scheme/gui)

; XXX unit bugs
(define-signature text-world^
  (scene? image? 
          (contracted
           #;[scene? contract?]
           #;[image? contract?]
           [place-image! (any/c #;image? exact-nonnegative-integer? exact-nonnegative-integer? any/c #;scene? . -> . void)]
           [place-image*! (string? exact-nonnegative-integer? exact-nonnegative-integer? (is-a?/c color%) (is-a?/c color%) any/c #;scene? . -> . void)]
           [make-image (char? (is-a?/c color%) (is-a?/c color%) . -> . any/c #;image?)]
           [big-bang (any/c
                      #:height exact-positive-integer?
                      #:width exact-positive-integer?
                      #:on-tick (any/c . -> . any/c)
                      #:tick-rate number?
                      #:on-key (any/c (is-a?/c key-event%) . -> . any/c)
                      #:on-draw (any/c any/c #;scene? . -> . void)
                      #:stop-when (any/c . -> . boolean?)
                      #:stop-timer (any/c . -> . boolean?)
                      . -> . 
                      any/c)])))

(provide text-world^)
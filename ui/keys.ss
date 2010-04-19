#lang scheme/gui
(define (true-key? key)
  (define kc (send key get-key-code))
  (not (and (symbol? kc) (symbol=? kc 'release))))

(define (key->action key)
  (define kc (send key get-key-code))
  (case kc
    [(#\k numpad8 up) 'up]
    [(#\j numpad2 down) 'down]
    [(#\h numpad4 left) 'left]
    [(#\l numpad6 right) 'right]
    [(#\y numpad7) 'left-up]
    [(#\u numpad9) 'right-up]
    [(#\b numpad1) 'left-down]
    [(#\n numpad3) 'right-down]
    [(#\i) 'inventory]
    [(#\o) 'open]
    [(#\c) 'close]
    [(#\t) 'throw]
    [(#\,) 'pickup]
    [(#\d) 'drop]
    [(#\q) 'quit]
    [(release) 'ignore]
    [else 'unknown]))

(provide/contract
 [true-key? ((is-a?/c key-event%) . -> . boolean?)]
 [key->action ((is-a?/c key-event%) . -> . symbol?)])
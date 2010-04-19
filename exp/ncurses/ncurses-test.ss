#lang scheme
(require "ncurses.ss")

(initialize-screen!)
(unless (has-colors?)
  (restore-screen!)
  (error 'ncurses "No colors!"))  
(raw!)
(no-newline-input!)
(no-echo!)
(set-keypad! stdscr #t)
(set-interrupt-flush! stdscr #f)
(set-meta! stdscr #t)
(start-color!)
(init-pair! 1 COLOR_RED COLOR_BLACK)

#;(printw "Type any character to see it in bold\n")
(define ch1 (get-char!))

(beep!)

(for ([ri (in-range 0 10)])
  (for ([ci (in-range 0 5)])
    (move! ri ci)
    (add-char! #\space)))

(refresh!)

(define chx (get-char!))

(for ([ri (in-range 0 10)])
  (for ([ci (in-range 0 5)])
    (move! ri ci)
    (attr-on! A_BOLD)
    (add-char! #\a)
    (attr-off! A_BOLD)))

(refresh!)

(define ch2 (get-char!))
(flash!)

(for ([ri (in-range 0 10)])
  (for ([ci (in-range 0 5)])
    (move! ri ci)
    (when (zero? (random 2))
      (delete-char!))))

(for ([ri (in-range 10 20)])
  (for ([ci (in-range 5 15)])
    (move! ri ci)
    (attr-on! (color-pair 1))
    (add-char! #\c)
    (attr-off! (color-pair 1))))

(refresh!)

(define row #f #;(get-cursor-row stdscr))
(define col #f #;(get-cursor-col stdscr))

(define ch3 (get-char!))

(restore-screen!)

(printf "~S~n" (list ch1 ch2 ch3
                     row col))
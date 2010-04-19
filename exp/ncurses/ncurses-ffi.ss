#lang scheme
(require scheme/foreign
         (prefix-in c: scheme/contract)
         "ncurses-constants.ss")
(unsafe!)

(provide (all-from-out "ncurses-constants.ss"))

; XXX wide version?
(define ncurses-lib (ffi-lib "libncurses"))

(define-syntax-rule (define-ffi f type contract)
  (begin
    (define f (get-ffi-obj 'f ncurses-lib type))
    (provide/contract
     [f contract])))

(define-syntax-rule (define-noarg-ffi f ...)
  (begin
    (define-ffi f (_fun -> _int) (c:-> exact-integer?))
    ...))

(define-syntax-rule (define-winarg-ffi f ...)
  (begin
    (define-ffi f (_fun _window -> _int) (window? . c:-> . exact-integer?))
    ...))

(define attribute? exact-integer?)
(define short? exact-integer?)
(define-cpointer-type _window)
(provide short? attribute? window?)

(define _attribute _int)

; curs_color
(define-noarg-ffi start_color)
(define-ffi init_pair (_fun _short _short _short -> _int) (short? short? short? . c:-> . exact-integer?))
(define-ffi init_color (_fun _short _short _short _short -> _int) (short? short? short? short? . c:-> . exact-integer?))
(define-ffi has_colors (_fun -> _bool) (c:-> boolean?))
(define-ffi can_change_color (_fun -> _bool) (c:-> boolean?))
(define-ffi color_content (_fun _short (r : (_ptr o _short)) (g : (_ptr o _short)) (b : (_ptr o _short)) 
                                -> (ret : _int)
                                -> (values r g b ret))
  (short? . c:-> . (values short? short? short? exact-integer?)))
(define-ffi pair_content (_fun _short (f : (_ptr o _short)) (b : (_ptr o _short))
                               -> (ret : _int)
                               -> (values f b ret))
  (short? . c:-> . (values short? short? exact-integer?)))
(define-ffi COLOR_PAIR (_fun _int -> _attribute) (exact-integer? . c:-> . attribute?))

; curs_move
(define-ffi move (_fun _int _int -> _int) (exact-integer? exact-integer? . c:-> . exact-integer?))
(define-ffi wmove (_fun _window _int _int -> _int) (window? exact-integer? exact-integer? . c:-> . exact-integer?))

; curs_attr
(define-ffi attrset (_fun _attribute -> _int) (attribute? . c:-> . exact-integer?))
(define-ffi attron (_fun _attribute -> _int) (attribute? . c:-> . exact-integer?))
(define-ffi attroff (_fun _attribute -> _int) (attribute? . c:-> . exact-integer?))
(define-ffi wattrset (_fun _window _attribute -> _int) (window? attribute? . c:-> . exact-integer?))
(define-ffi wattron (_fun _window _attribute -> _int) (window? attribute? . c:-> . exact-integer?))
(define-ffi wattroff (_fun _window _attribute -> _int) (window? attribute? . c:-> . exact-integer?))

; curs_addch
(define-ffi addch (_fun _byte -> _int) (byte? . c:-> . exact-integer?))
(define-ffi waddch (_fun _window _byte -> _int) (window? byte? . c:-> . exact-integer?))

; curs_beep
(define-noarg-ffi beep flash)

; curs_clear
(define-noarg-ffi erase clear clrtobot clrtoeol)
(define-winarg-ffi werase wclear wclrtobot wclrtoeol)

; curs_outopts
(define-noarg-ffi nl nonl)

; curs_delch
(define-noarg-ffi delch)
(define-winarg-ffi wdelch)

; curs_initscr
(define-ffi initscr (_fun -> _window) (c:-> window?))
(define-noarg-ffi endwin)

; curs_refresh
(define-noarg-ffi refresh)
(define-winarg-ffi wrefresh)

; curs_legacy
; XXX not in os x
#;(define-ffi getcurx (_fun _window -> _int) (window? . c:-> . exact-integer?))
#;(define-ffi getcury (_fun _window -> _int) (window? . c:-> . exact-integer?))

; curs_getch
(define-ffi getch (_fun -> _byte) (c:-> byte?))
(define-ffi wgetch (_fun _window -> _byte) (window? . c:-> . byte?))

; curs_inopts
(define-noarg-ffi
  cbreak nocbreak
  echo noecho
  raw noraw)
(define-ffi intrflush (_fun _window _bool -> _int) (window? boolean? . c:-> . exact-integer?))
(define-ffi keypad (_fun _window _bool -> _int) (window? boolean? . c:-> . exact-integer?))
(define-ffi meta (_fun _window _bool -> _int) (window? boolean? . c:-> . exact-integer?))

; Other
(define-c stdscr ncurses-lib _window)
(provide stdscr)

; curs_kernel
(define-ffi curs_set (_fun _int -> _int) ((one-of/c 0 1 2) . c:-> . exact-integer?))

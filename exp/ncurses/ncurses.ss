#lang scheme
; XXX rename on out
(require "ncurses-constants.ss")
(provide (all-from-out "ncurses-constants.ss"))

(require "ncurses-ffi.ss")
          
(define-struct (exn:fail:ncurses exn:fail) ())
(define (check-error ret f . args)
  ; XXX why is this breaking?
  (when #f #;(= ret ERR)
    (raise 
     (make-exn:fail:ncurses
      (format "ncurses error in ~a with ret ~e and args: ~e" f ret args)
      (current-continuation-marks)))))

(define (identity x) x)

(define-syntax (define-check-error/coerce stx)
  (syntax-case stx ()
    [(_ fun ffi [arg/c arg-coerce] ...)
     (with-syntax ([(arg-id ...) (generate-temporaries #'(arg/c ...))])
       (syntax/loc stx     
         (begin
           (define (fun arg-id ...)
             (check-error 
              (ffi (arg-coerce arg-id) ...)
              ffi arg-id ...))
           (provide/contract
            [fun (arg/c ... . -> . void)]))))]))

(define-syntax-rule (define-check-error fun ffi arg/c ...)
  (define-check-error/coerce fun ffi [arg/c identity] ...))

(define-syntax-rule (define-rename fun ffi)
  (provide
   (rename-out [ffi fun])))

; curs_color
(define-check-error start-color! start_color)
(define-check-error init-pair! init_pair short? short? short?)
(define-check-error init-color! init_color short? short? short? short?)
(define-rename has-colors? has_colors)
(define-rename can-change-color? can_change_color)

(define (color-content c)
  (define-values (r g b ret) (color_content c))
  (check-error ret color-content c)
  (values r g b))
(provide/contract
 [color-content (short? . -> . (values short? short? short?))])

(define (pair-content c)
  (define-values (f b ret) (pair_content c))
  (check-error ret pair-content c)
  (values f b))
(provide/contract
 [pair-content (short? . -> . (values short? short?))])

(define-rename color-pair COLOR_PAIR)

; curs_move
(define-check-error move! move exact-integer? exact-integer?)
(define-check-error window-move! wmove window? exact-integer? exact-integer?)

; curs_attr
(define-check-error attr-set! attrset attribute?)
(define-check-error attr-on! attron attribute?)
(define-check-error attr-off! attroff attribute?)
(define-check-error window-attr-set! wattrset window? attribute?)
(define-check-error window-attr-on! wattron window? attribute?)
(define-check-error window-attr-off! wattroff window? attribute?)

; curs_addch
(define-check-error/coerce add-char! addch [char? char->integer])
(define-check-error/coerce window-add-char! waddch [window? identity] [char? char->integer])

; curs_beep
(define-check-error beep! beep)
(define-check-error flash! flash)

; curs_clear
(define-check-error erase! erase)
(define-check-error clear! clear)
(define-check-error clear-to-bottom! clrtobot)
(define-check-error clear-to-end! clrtoeol)
(define-check-error window-erase! werase window?)
(define-check-error window-clear! wclear window?)
(define-check-error window-clear-to-bottom! wclrtobot window?)
(define-check-error window-clear-to-end! wclrtoeol window?)

; curs_outopts
(define-check-error newline-input! nl)
(define-check-error no-newline-input! nonl)

; curs_delch
(define-check-error delete-char! delch)
(define-check-error window-delete-char! wdelch window?)

; curs_initscr
(define (initialize-screen!)
  (initscr)
  (void))
(provide/contract
 [initialize-screen! (-> void)])

(define-check-error restore-screen! endwin)

; curs_refresh
(define-check-error refresh! refresh)
(define-check-error window-refresh! wrefresh window?)

; curs_legacy
#;(define-rename get-cursor-col getcurx)
#;(define-rename get-cursor-row getcury)

; curs_getch
(define (get-char!)
  (integer->char (getch)))
(define (window-get-char! w)
  (integer->char (wgetch w)))
(provide/contract
 [get-char! (-> char?)]
 [window-get-char! (window? . -> . char?)])

; curs_inopts
(define-check-error carriage-break! cbreak)
(define-check-error no-carriage-break! nocbreak)
(define-check-error echo! echo)
(define-check-error no-echo! noecho)
(define-check-error raw! raw)
(define-check-error no-raw! noraw)

(define-check-error set-interrupt-flush! intrflush window? boolean?)
(define-check-error set-keypad! keypad window? boolean?)
(define-check-error set-meta! meta window? boolean?)

(define (symbol->cursor-visibility s)
  (case s
    [(invisible) 0]
    [(normal) 1]
    [(very-visible) 2]))
(define-check-error/coerce set-cursor! curs_set [(symbols 'invisible 'normal 'very-visible) symbol->cursor-visibility])

; Other
(provide stdscr)
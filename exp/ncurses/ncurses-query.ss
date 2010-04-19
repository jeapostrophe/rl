#lang scheme
(printf #<<END
#include <ncurses.h>
#include <stdio.h>

int main() {
END
        )

(printf "printf(\"#lang scheme\\n\");~n")
(for ([a (in-list '(ERR
                    A_NORMAL A_ATTRIBUTES A_CHARTEXT A_COLOR A_STANDOUT A_UNDERLINE A_REVERSE A_BLINK A_DIM A_BOLD A_ALTCHARSET A_INVIS 
                    A_PROTECT A_HORIZONTAL A_LEFT A_LOW A_RIGHT A_TOP A_VERTICAL 
                    COLOR_BLACK COLOR_RED COLOR_GREEN COLOR_YELLOW COLOR_BLUE COLOR_MAGENTA COLOR_CYAN COLOR_WHITE ACS_ULCORNER))])
  (printf "printf(\"(define ~a %d) (provide/contract [~a exact-integer?])\\n\", ~a);~n" a a a))

(printf #<<END
}
END
        )


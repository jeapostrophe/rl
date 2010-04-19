CFLAGS=-lncurses

ncurses-constants.ss: ncurses-query
	./$^ > $@

ncurses-query.c: ncurses-query.ss
	mzscheme -t $^ > $@


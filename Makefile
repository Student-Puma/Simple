.PHONY: all, build, clean, test

all: build
build: simple

simple: simple.tab.c simple.lex.c
	gcc -o simple -lm $^
simple.tab.c:	simple.y
	bison -dv simple.y
simple.lex.c: simple.l
	flex -o $@ $<

test:
	./simple examples/ordenar.sim

clean:
	@rm simple simple.lex.c simple.tab.c simple.tab.h simple.output

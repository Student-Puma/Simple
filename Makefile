.PHONY: all, bison, build, clean, lex, simple

all: build
build: simple
simple: bison

bison: simple.tab.c simple.lex.c
	gcc -o simple -lm $^
lex: simple.lex.c
	gcc -o simple $<

simple.tab.c:	simple.y
	bison -dv simple.y
simple.lex.c: simple.l
	flex -o $@ $<

clean:
	@rm simple simple.lex.c simple.tab.c simple.tab.h simple.output

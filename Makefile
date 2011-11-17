CC=gcc
CC_FLAGS=-Wall -ansi -pedantic-errors -I include/

all: bin object bin/tokenize

bin:
	mkdir bin

object:
	mkdir object

bin/tokenize: src/bin/tokenize.cpp include/core/psil_core.hpp include/core/token/token.hpp include/core/token/tokenizer.hpp
	${CC} ${CC_FLAGS} -o ${@} ${<}

clean:
	rm bin/* 

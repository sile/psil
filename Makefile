CC=g++
CC_FLAGS=-Wall -ansi -pedantic-errors -I include/

all: bin object bin/tokenize

bin:
	mkdir bin

object:
	mkdir object

bin/tokenize: src/bin/tokenize.cc include/core/psil_core.hh include/core/token/token.hh include/core/token/tokenizer.hh include/core/util/char_stream.hh
	${CC} ${CC_FLAGS} -o ${@} ${<}

clean:
	rm bin/* 

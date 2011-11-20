CC=g++
CC_FLAGS=-Wall -ansi -pedantic-errors -I include/

all: bin object bin/tokenize bin/eval

bin:
	mkdir bin

object:
	mkdir object

bin/tokenize: src/bin/tokenize.cc include/core/psil_core.hh object/tokenizer.o
	${CC} ${CC_FLAGS} -o ${@} ${<} object/tokenizer.o

bin/eval: src/bin/eval.cc include/core/psil_core.hh object/eval.o object/tokenizer.o
	${CC} ${CC_FLAGS} -o ${@} ${<} object/eval.o object/tokenizer.o

object/tokenizer.o: src/core/token/tokenizer.cc include/core/token/token.hh include/core/token/tokenizer.hh include/core/util/char_stream.hh
	${CC} ${CC_FLAGS} -c -o ${@} ${<} 

object/eval.o: src/core/eval/evaluator.cc include/core/eval/evaluator.hh include/core/token/token.hh
	${CC} ${CC_FLAGS} -c -o ${@} ${<}

clean:
	rm bin/*
	rm object/* 

CC=g++
CC_FLAGS=-Wall -ansi -pedantic-errors -I include/

INC=include/core

all: bin bin/psil

bin:
	mkdir bin

bin/psil: src/bin/psil.cc ${INC}/psil.hh
	${CC} ${CC_FLAGS} -o ${@} ${<}

include/core/psil.hh: ${INC}/interpreter.hh ${INC}/bytecode_reader.hh ${INC}/bytecode.hh ${INC}/symbol_table.hh ${INC}/object.hh ${INC}/bindings.hh ${INC}/util.hh
	touch ${@}

clean:
	rm bin/*

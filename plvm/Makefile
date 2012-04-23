CXX = g++
CXXFLAGS = -O2 -Wall -Iinclude

SRC = $(shell find src -name '*.cc')
OBJ = $(SRC:.cc=.o)

.PHONY: all clean
.SUFFIXES: .cc .o

all: bin/plvm $(OBJS)

clean:
	rm bin/*
	rm `find . -name '*.o'`

bin/plvm: $(OBJ)
	mkdir -p bin
	$(CXX) $(CXXFLAGS) -o $@ $(OBJ)

.cc.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

dep:
	./gen_dep.sh $(SRC) > makefile.depend

-include makefile.depend

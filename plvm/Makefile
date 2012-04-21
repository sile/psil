CXX = g++
CXXFLAGS = -Wall -Iinclude

SRC = $(shell find src -name '*.cc')
OBJ = $(SRC:.cc=.o)

.PHONY: all clean
.SUFFIXES: .cc .o

all: bin/plvm $(OBJS)

clean:
	rm bin/*
	rm `find . -name '*.o'`

bin/plvm: $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJ)

.cc.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

dep:
	./gen_dep.sh $(SRC) > makefile.depend

-include makefile.depend

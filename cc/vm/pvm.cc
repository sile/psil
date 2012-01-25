#include "pvm.hh"
#include <iostream>

int main(int argc, char** argv) {
  if(argc != 2) {
    std::cerr << "Usage: pvm BYTECODE_FILEPATH" << std::endl;
    return 1;
  }
  
  pvm::executor vm;
  vm.execute(argv[1]);
  vm.get_env().show(std::cout);

  return 0;
}

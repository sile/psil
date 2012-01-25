#include "pvm.hh"
#include <iostream>

int main(int argc, char** argv) {
  if(argc != 2) {
    std::cerr << "Usage: pvm BYTECODE_FILEPATH" << std::endl;
    return 1;
  }
  
  pvm::executor vm;
  vm.execute(argv[1]);

  const pvm::environment& rlt = vm.get_env();
  std::cout << "[data stack]" << std::endl;
  for(int i = rlt.dstack().size()-1; i >= 0; i--) {
    std::cout << " " << (rlt.dstack().size()-1-i) << "# " << rlt.dstack()[i] << std::endl;
  }
  std::cout << std::endl;

  std::cout << "[return stack]" << std::endl;
  for(int i = rlt.rstack().size()-1; i >= 0; i--) {
    std::cout << " " << (rlt.rstack().size()-1-i) << "# " << rlt.rstack()[i] << std::endl;
  }
  std::cout << std::endl;

  return 0;
}

#include "pvm.hh"
#include <iostream>

void show(const char* name, const pvm::stack_t& stack) {
  std::cout << "[" << name << "]" << std::endl;
  for(int i = stack.size()-1; i >= 0; i--) {
    std::cout << " " << (stack.size()-1-i) << "# " << stack[i] << std::endl;
  }
  std::cout << std::endl;  
}

int main(int argc, char** argv) {
  if(argc != 2) {
    std::cerr << "Usage: pvm BYTECODE_FILEPATH" << std::endl;
    return 1;
  }
  
  pvm::executor vm;
  vm.execute(argv[1]);

  const pvm::environment& rlt = vm.get_env();
  show("data stack", rlt.dstack());
  show("return stack", rlt.rstack());

  return 0;
}

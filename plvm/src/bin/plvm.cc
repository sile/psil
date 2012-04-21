#include "psil/vm/Environment.hh"
#include "psil/vm/Executor.hh"
#include "psil/vm/aux.hh"
#include <iostream>

using namespace psil;

int main(int argc, char** argv) {
  if(argc == 1) {
    std::cerr << "Usage: plvm BYTECODE_FILEPATH [BYTECODE_FILEPATH*]" << std::endl;
    return 1;
  }
  
  vm::Environment env;
  vm::Executor exec(env);
  
  for(int i=1; i < argc; i++) {
    const char* filepath = argv[i];
    std::cout << "# " << filepath << std::endl;
    
    vm::aux::SmartPtr<vm::BytecodeObject> bcp(vm::FileBytecodeObject::parse(filepath));
    if(bcp.isNull()) {
      std::cerr << "ERROR: can't parse " << filepath << std::endl;
      return 1;
    }

    if(exec.execute(bcp.getRef())) {
      std::cerr << "ERROR: can't execute " << filepath << std::endl;
      return 1;
    }

    exec.printState();
    std::cout << std::endl;
  }
  
  return 0;
}

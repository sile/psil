#include "psil/vm/Environment.hh"
#include "psil/vm/Executor.hh"
#include "psil/vm/FileContent.hh"
#include "psil/vm/ByteStream.hh"
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
    
    vm::FileContent fc(filepath);
    if(! fc) {
      std::cerr << "ERROR: can't open file" << std::endl;
      return 1;
    }
    
    vm::ByteStream bs(fc.bytes(), fc.size());
    vm::BytecodeObject bo(bs);
    if(! bo) {
      std::cerr << "ERROR: can't parse " << filepath << std::endl;
      return 1;
    }
    std::cout << bo.show() << std::endl << std::endl;

    if(! exec.execute(bo)) {
      std::cerr << "ERROR: can't execute " << filepath << std::endl;
      return 1;
    }

    std::cout << exec.show();
    std::cout << std::endl;
  }
  
  return 0;
}

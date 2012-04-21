#include "psil/vm/Environment.hh"
#include "psil/vm/Executor.hh"
#include <iostream>

int main(int argc, char** argv) {
  if(argc == 1) {
    std::cerr << "Usage: plvm BYTECODE_FILEPATH [BYTECODE_FILEPATH*]" << std::endl;
    return 1;
  }
  
  psil::vm::Environment env;
  psil::vm::Executor exec(env);
  
  try {
    for(int i=1; i < argc; i++) {
      const char* filepath = argv[i];
      std::cout << "# " << filepath << std::endl;

      psil::vm::FileBytecodeObject bc(filepath);
      exec.execute(bc);
      exec.printState();
      std::cout << std::endl;
    }
  } catch (const std::exception& ex) {
    std::cerr << "[ERROR]" << std::endl << ex.what() << std::endl;
    return 1;
  }
  
  return 0;
}

#include "core/psil.hh"
#include <iostream>
#include <fstream>

int main(int argc, char** argv) {
  psil::core::interpreter intr;

  if(argc == 1) {
    intr.interpret(std::cin);
  } else {
    for(int i=1; i < argc; i++) {
      std::cout << "[[[" << argv[i] << "]]]" << std::endl;
      std::ifstream in(argv[i]);
      if(!in) {
        std::cerr << "Can't open file: " << argv[i] << std::endl;
        return 1;
      }
      intr.interpret(in);
      std::cout << std::endl;
    }
  }
  return 0;
}

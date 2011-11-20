#include "core/psil_core.hh"
#include "core/util/mmap_t.hh"
#include <iostream>

using namespace psil::core::eval;
using namespace psil::core::token;
using namespace psil::core::util;

int main(int argc, char** argv) {
  if(argc != 2) {
    std::cerr << "Usage: eval SOURCE_FILE" << std::endl;
    return 1;    
  }

  const char* sourcepath = argv[1];
  mmap_t<char> mm(sourcepath);
  if(!mm) {
    std::cerr << "Can't open file: " << sourcepath << std::endl;
    return 1;
  }

  Tokenizer tkn(mm.start(), mm.end());
  std::cout << Evaluator().eval(*tkn.tokenize())->toString() << std::endl;

  return 0;
}

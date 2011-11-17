#include "core/psil_core.hh"
#include "core/util/mmap_t.hh"
#include <iostream>

using namespace psil::core::token;
using namespace psil::core::util;

int main(int argc, char** argv) {
  if(argc != 2) {
    std::cerr << "Usage: tokenize SOURCE_FILE" << std::endl;
    return 1;    
  }

  const char* sourcepath = argv[1];
  mmap_t<char> mm(sourcepath);
  if(!mm) {
    std::cerr << "Can't open file: " << sourcepath << std::endl;
    return 1;
  }

  CharStream in(mm.start(), mm.end());
  Tokenizer tkn;
  tkn.tokenize(in, 0);

  return 0;
}

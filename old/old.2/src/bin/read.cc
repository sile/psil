#include "core/psil_core.hh"
#include "core/util/mmap_t.hh"
#include <iostream>

using namespace psil::core::read;
using namespace psil::core::object;
using namespace psil::core::util;

int main(int argc, char** argv) {
  if(argc != 2) {
    std::cerr << "Usage: reader SOURCE_FILE" << std::endl;
    return 1;    
  }

  const char* sourcepath = argv[1];
  mmap_t<char> mm(sourcepath);
  if(!mm) {
    std::cerr << "Can't open file: " << sourcepath << std::endl;
    return 1;
  }

  Reader reader;
  std::cout << reader.read(mm.start(), mm.end())->toString() << std::endl;

  return 0;
}

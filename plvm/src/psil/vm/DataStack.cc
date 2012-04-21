#include "psil/vm/DataStack.hh"
#include <sstream>

using namespace psil::vm;

std::string DataStack::show() const {
  std::ostringstream out;
  out << "[DataStack]" << std::endl;
  for(std::size_t i=0; i < stack.size(); i++) {
    std::size_t j = stack.size()-i;
    if(j == base) {
      out << "--- base ---" << std::endl;
    }
    out << " " << j << ": " << stack[j]->show() << std::endl;
  }
  return out.str();
}

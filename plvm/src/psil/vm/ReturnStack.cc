#include "psil/vm/ReturnStack.hh"
#include <sstream>

using namespace psil::vm;

std::string ReturnStack::show() const {
  std::ostringstream out;
  out << "[ReturnStack]" << std::endl;
  for(std::size_t i=0; i < stack.size(); i++) {
    std::size_t j = stack.size()-i;
    const Entry& e = stack[i];
    out << " " << j << ": " << e.base << ", " << e.returnAddress << std::endl;
  }
  return out.str();
}

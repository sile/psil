#include "psil/vm/DataStack.hh"
#include <sstream>
#include <iostream>
using namespace psil::vm;

std::string DataStack::show() const {
  std::ostringstream out;
  out << "[DataStack]" << std::endl;
  out << "--- top ---" << std::endl;
  for(std::size_t i=0; i < stack.size(); i++) {
    std::size_t j = stack.size()-i-1;
    // XXX: reserve時にNULLになることがある => あまり良くない
    out << " " << j << ": " << (stack[j] ? stack[j]->show() : "NULL") << std::endl;
    if(j == base) {
      out << "--- base ---" << std::endl;
    }
  }
  return out.str();
}

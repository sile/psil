#include "psil/vm/Object.hh"
#include "psil/vm/SymbolTable.hh"

#include "psil/vm/type.hh"
#include <sstream>

using namespace psil::vm;
using namespace psil::vm::type;

Undef Undef::undef;

Symbol* Symbol::make(const std::string& name) {
  Symbol* sym = SymbolTable::find(name);
  if(sym != NULL) {
    return sym;
  }
  return SymbolTable::intern(new Symbol(name));
}

std::string Int::show() const {
  std::ostringstream out;
  out << "<INT " << value << ">";
  return out.str();
}

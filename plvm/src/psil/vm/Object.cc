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

std::string Char::show() const {
  std::ostringstream out;
  out << "<CHAR " << code << ">";
  return out.str();
}

Nil Nil::nil;

Boolean Boolean::_true;
Boolean Boolean::_false;

std::string Cons::show() const {
  std::ostringstream out;
  out << "<CONS " << car->show() << " " << cdr->show() << ">";
  return out.str();
}

std::string Array::show() const {
  std::ostringstream out;
  out << "<ARRAY";
  for(uint4 i=0; i < size; i++) {
    out << " " << get(i)->show();
  }
  out << ">";
  return out.str();
}

std::string Lambda::show() const {
  std::ostringstream out;
  out << "<LAMBDA " 
      << (int)closed.val_count << " " 
      << (int)arity << " "
      << (int)local_var_count
      << ">";
  return out.str();
}

std::string Port::show() const {
  std::ostringstream out;
  out << "<PORT " << fd << ">";
  return out.str();
}

Port Port::STDIN(0, true);
Port Port::STDOUT(1, false);
Port Port::STDERR(2, false);
Port* Port::CURRENT_INPUT = &Port::STDIN;
Port* Port::CURRENT_OUTPUT = &Port::STDOUT;
Symbol* Port::END_OF_FILE = Symbol::make("eof");

#ifndef PSIL_VM_SYMBOL_TABLE_HH
#define PSIL_VM_SYMBOL_TABLE_HH

#include "Object.hh"
#include <string>

namespace psil {
  namespace vm {
    class SymbolTable {
    public:
      type::Symbol* intern(const std::string& name) {
        return NULL;
      }
    };
  }
}

#endif

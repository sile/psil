#ifndef PSIL_VM_SYMBOL_TABLE_HH
#define PSIL_VM_SYMBOL_TABLE_HH

#include "Object.hh"
#include <string>
#include <tr1/unordered_map>

namespace psil {
  namespace vm {
    typedef std::tr1::unordered_map<std::string, type::Symbol*> symbol_table_t;

    class SymbolTable {
    public:
      static type::Symbol* find(const std::string& name) {
        symbol_table_t::iterator rlt = table.find(name);
        return rlt == table.end() ? NULL : rlt->second;
      }

      static type::Symbol* intern(type::Symbol* symbol) {
        type::Symbol* cur = find(symbol->name);
        if(cur == NULL) {
          table[symbol->name] = symbol;
          cur = symbol;
        }
        return cur;
      }

    private:
      static symbol_table_t table;
    };
  }
}

#endif

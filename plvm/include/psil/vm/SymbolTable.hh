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
        symbol_table_t::iterator rlt = getTable().find(name);
        return rlt == getTable().end() ? NULL : rlt->second;
      }

      static type::Symbol* intern(type::Symbol* symbol) {
        type::Symbol* cur = find(symbol->getName());
        if(cur == NULL) {
          getTable()[symbol->getName()] = symbol;
          cur = symbol;
        }
        return cur;
      }

    private:
      static symbol_table_t& getTable() {
        static symbol_table_t table;
        return table;
      }
      // static symbol_table_t table;
    };
  }
}

#endif

#ifndef PSIL_CORE_SYMBOL_TABLE_HH
#define PSIL_CORE_SYMBOL_TABLE_HH

#include "object.hh"
#include <vector>

namespace psil {
  namespace core {
    class symbol_table {
    public:
      struct entry {
        obj::symbol* sym;
        obj::string* name;
      };

    public:
      symbol_table() {
        
        // XXX:
        // reserved
        {
          entry e = {(obj::symbol*)obj::o_nil(), new obj::string("NIL")};
          table.push_back(e);
        }

        // XXX:
        // reserved
        {
          entry e = {(obj::symbol*)obj::o_t(), new obj::string("T")};
          table.push_back(e);
        }
      }
      const obj::string* symbol_name(int symbol_code) const {
        for(unsigned i=0; i < table.size(); i++) 
          if(table[i].sym->value() == symbol_code)
            return table[i].name;
        return NULL;
      }
      
      obj::symbol* symbol_by_name(const obj::string& symbol_name) const {
        for(unsigned i=0; i < table.size(); i++) 
          if(*table[i].name == symbol_name) {
            return table[i].sym;
          }
        return NULL;
      }

      obj::symbol* symbol_by_code(int symbol_code) const {
        for(unsigned i=0; i < table.size(); i++) 
          if(table[i].sym->value() == symbol_code)
            return table[i].sym;
        return NULL;
      }

      void add(obj::symbol* sym, obj::string* name) {
        entry e = {sym, name};
        table.push_back(e);
      }
      
      obj::symbol* intern(obj::string* name) {
        obj::symbol* sym = symbol_by_name(*name);
        if(sym)
          return sym;
        
        sym = obj::symbol::create_from_code(table.size());
        add(sym, name);
        return sym;
      }
      
      int size() const { return table.size(); }
      
      const entry& get_entry(int i) const { return table[i]; }
    public:
      static symbol_table *g_table; // XXX:
      
    private:
      std::vector<entry> table;
    };
    symbol_table* symbol_table::g_table = NULL;
  }
}

#endif

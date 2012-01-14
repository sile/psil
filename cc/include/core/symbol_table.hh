#ifndef PSIL_CORE_SYMBOL_TABLE_HH
#define PSIL_CORE_SYMBOL_TABLE_HH

#include "object.hh"
#include <vector>

namespace psil {
  namespace core {
    class symbol_table {
    public:
      struct entry {
        int code;
        obj::string* name;
      };

    public:
      const obj::string* symbol_name(int symbol_code) const {
        for(unsigned i=0; i < table.size(); i++) 
          if(table[i].code == symbol_code)
            return table[i].name;
        return NULL;
      }
      
      const int* symbol_code(const obj::string& symbol_name) const {
        for(unsigned i=0; i < table.size(); i++) 
          if(*table[i].name == symbol_name)
            return &table[i].code;
        return NULL;
      }

      void add(int code, obj::string* name) {
        entry e = {code, name};
        table.push_back(e);
      }
      
      int size() const { return table.size(); }
      
      const entry& get_entry(int i) const { return table[i]; }
    private:
      std::vector<entry> table;
    };
  }
}

#endif

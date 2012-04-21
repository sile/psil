#ifndef PSIL_VM_CONSTANT_TABLE_HH
#define PSIL_VM_CONSTANT_TABLE_HH

#include "aux.hh"
#include "Object.hh"
#include <cstddef>
#include <vector>
#include <string>
#include <sstream>

/*
  [format] // 暫定. 現状シンボルのみ
  len:2byte name:len-byte *
 */
namespace psil {
  namespace vm {
    class ConstantTable {
    public:
      static ConstantTable* parse(const char* bytes, unsigned count) {
        ConstantTable* consts = new ConstantTable;
        for(unsigned i=0; i < count; i++) {
          short length = aux::Endian::toBigShort(*reinterpret_cast<const unsigned*>(bytes));
          std::string name(bytes+2, length);
          consts->table.push_back(type::Symbol::intern(name));
          bytes += length;
        }
        return consts;
      }

      std::string show() const {
        std::ostringstream out;
        out << "[ConstantTable]" << std::endl;
        for(std::size_t i=0; i < table.size(); i++) {
          out << " " << i << ": " << table[i]->show() << std::endl;
        }
        return out.str();
      }
      
    private:
      ConstantTable() {}

    private:
      std::vector<type::Symbol*> table;
    };
  }
}

#endif

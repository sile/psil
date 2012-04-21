#ifndef PSIL_VM_CONSTANT_TABLE_HH
#define PSIL_VM_CONSTANT_TABLE_HH

#include "ByteStream.hh"
#include "Object.hh"
#include <vector>
#include <string>

/*
  [format] // 暫定. 現状シンボルのみ
  len:2byte name:len-byte *
 */
namespace psil {
  namespace vm {
    class ConstantTable {
    public:
      ConstantTable(ByteStream& in, unsigned count);
      std::string show() const;

      type::Object* get(unsigned index) const { return table[index]; }
      
    private:
      std::vector<type::Symbol*> table;
    };
  }
}

#endif

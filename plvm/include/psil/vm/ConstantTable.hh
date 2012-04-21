#ifndef PSIL_VM_CONSTANT_TABLE_HH
#define PSIL_VM_CONSTANT_TABLE_HH

#include <cstddef>

namespace psil {
  namespace vm {
    class ConstantTable {
    public:
      ConstantTable() {}

      static ConstantTable* parse(const char* bytes, unsigned count) {
        return NULL;
      }
    };
  }
}

#endif

#ifndef PSIL_CORE_BYTECODE_READER_HH
#define PSIL_CORE_BYTECODE_READER_HH

#include "bytecode.hh"
#include <iostream>

namespace psil {
  namespace core {
    class bytecode_reader {
    public:
      bytecode_reader(std::istream& in) : in(in) {}

      bytecode::TYPE read_type() {
        return bytecode::TYPE_INT;
      }

      void read_header(bytecode::header& hdr) {
        read_int(&hdr.version);
        read_int(&hdr.symbol_count);
      }

    private:
      void read_int(int* ptr) {
        in.read(reinterpret_cast<char*>(ptr), sizeof(int));
      }
      
    private:
      std::istream& in;
    };
  }
}

#endif

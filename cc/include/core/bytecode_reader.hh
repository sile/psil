#ifndef PSIL_CORE_BYTECODE_READER_HH
#define PSIL_CORE_BYTECODE_READER_HH

#include "bytecode.hh"
#include "symbol_table.hh"
#include "bindings.hh"
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

      void read_symbol_table(const bytecode::header& hdr, symbol_table& table) {
        int code;
        std::string name;
        for(int i=0; i < hdr.symbol_count; i++) {
          read_int(&code);
          read_string(name);
          table.add(code, new obj::string(name.c_str()));
        }
      }

      void read_init_data(const bytecode::header& hdr, bindings& bindings) {
        
      }

    private:
      void read_int(int* ptr) {
        in.read(reinterpret_cast<char*>(ptr), sizeof(int));
      }

      void read_string(std::string& buf) {
        int len;
        read_int(&len);
        buf.resize(len);

        in.read(const_cast<char*>(buf.c_str()), len);
      }
      
    private:
      std::istream& in;
    };
  }
}

#endif

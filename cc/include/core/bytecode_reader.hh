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
        // version:4
        // symbol_count:4
        // data_count:4
        read_int(&hdr.version);
        read_int(&hdr.symbol_count);
        read_int(&hdr.data_count);
      }

      void read_symbol_table(const bytecode::header& hdr, symbol_table& table) {
        // XXX: 無意味になった
        int code;
        std::string name;
        for(int i=0; i < hdr.symbol_count; i++) {
          // code:4
          // length:4
          // string:length
          read_int(&code);
          read_string(name);
          //table.add(code, new obj::string(name.c_str()));
        }
      }

      void read_init_data(const bytecode::header& hdr, bindings& bindings) {
        for(int i=0; i < hdr.data_count; i++) {
          obj::symbol* sym = obj::to_symbol(read_object());
          bindings[sym->value()] = read_object();
        }
      }

      obj::object* read_object() {
        obj::object* o = obj::read_object(in);
        return o;
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

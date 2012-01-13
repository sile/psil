#ifndef PSIL_CORE_INTERPRETER_HH
#define PSIL_CORE_INTERPRETER_HH

#include "bytecode.hh"
#include "bytecode_reader.hh"
#include "symbol_table.hh"
#include "object.hh"
#include <iostream>
#include <string>

namespace psil {
  namespace core {
    class interpreter {
      typedef bytecode_reader reader;
      
    public:
      interpreter() {}
      
      void interpret(std::istream& in) {
        bytecode_reader in2(in);
        interpret(in2);
      }
      void interpret(reader& in) {
        bytecode::header hdr;
        in.read_header(hdr);
        
        std::cout << "version: " << hdr.version << std::endl;
        std::cout << "symbol count: " << hdr.symbol_count << std::endl;
        
        obj::string s("abc");
        std::string buf;
        s.show(buf);
        //std::cout << s.show(buf) << std::endl;
        std::cout << buf << std::endl;
        std::cout << s.length() << std::endl;
        /*
        in.read_initial_data();
        
        
        switch(in.read_type()) {
        case bytecode::TYPE_SYMBOL:
        case bytecode::TYPE_QUOTE:

        case bytecode::TYPE_INT:
        case bytecode::TYPE_NIL:
          break;
        }
        */
      }

    private:
      symbol_table symbols;
    };
  }
}

#endif

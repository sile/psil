#ifndef PSIL_CORE_INTERPRETER_HH
#define PSIL_CORE_INTERPRETER_HH

#include "bytecode.hh"
#include "bytecode_reader.hh"
#include "symbol_table.hh"
#include "object.hh"
#include "bindings.hh"
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
        // header
        bytecode::header hdr;
        in.read_header(hdr);
        
        std::cout << "# version: " << hdr.version << std::endl;
        
        // symbol-table
        in.read_symbol_table(hdr, symbols);
        
        std::string buf;
        std::cout << "# symbol: " << std::endl;
        for(int i=0; i < symbols.size(); i++) {
          std::cout << " # [" << symbols.get_entry(i).code << "]" 
                    << " " << symbols.get_entry(i).name->show(buf) << std::endl;
        }

        // initial-data
        in.read_init_data(hdr, g_binds);

        std::cout << "# data: " << std::endl;
        {
          bindings::const_iterator itr = g_binds.begin();
          for(; itr != g_binds.end(); ++itr) {
            std::cout << " # [" << itr->first << "] " << itr->second->show(buf) <<  std::endl;
          }
        }
        
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
      bindings g_binds;
    };
  }
}

#endif

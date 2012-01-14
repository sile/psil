#ifndef PSIL_CORE_INTERPRETER_HH
#define PSIL_CORE_INTERPRETER_HH

#include "util.hh"

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
        
        // interpret
        std::cout << "# interpret:" << std::endl;
        do_interpret(in);
      }

    private:
      void do_interpret(reader& in) {
        obj::object* o = in.read_object();
        std::cout << " # read: " << o->show(buf) << std::endl;
        
        obj::object* result;
        switch(o->type()) {
        case obj::O_LIST: 
          o = ((obj::list*)o)->value();
          if(o == &obj::NIL) {
            result = o;
            break;
          }
        case obj::O_CONS: 
          // TODO:
          break;
        
        case obj::O_REFER: 
          // TODO?
          break;

        case obj::O_SYMBOL: 
          result = symbol_value(reinterpret_cast<obj::symbol*>(o));
          break;
          
        case obj::O_QUOTE:
          result = ((obj::quote*)o)->value();
          break;
          
        case obj::O_OBJECT:          
        case obj::O_INTEGER:
        case obj::O_STRING: 
          result = o;
          break;
        }

        std::cout << " # result: " << result->show(buf) << std::endl;
      }
      
      obj::object* symbol_value(obj::symbol* sym) {
        if(g_binds.find(sym->value()) == g_binds.end())
          ERR(std::string("unbinded symbol: ")+sym->show(buf));
        return g_binds[sym->value()];
      }

    private:
      std::string buf;
      
      symbol_table symbols;
      bindings g_binds;
    };
  }
}

#endif

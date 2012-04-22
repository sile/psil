#ifndef PSIL_VM_INSTRUCTION_HH
#define PSIL_VM_INSTRUCTION_HH

#include "type.hh"
#include "Environment.hh"
#include "Object.hh"
#include <cassert>

namespace psil {
  namespace vm {
    class Instruction {
    public:
      Instruction(Environment& env) : env(env) {};
      
      bool hasNextOp() const {
        return env.getCodeStream().eos() == false;
      }

      void execute() {
        opcode_t op =  env.getCodeStream().readUint1();
        
        switch (op) {
        case   1: _int(); break;
        case   2: _string(); break;
        case   3: _char(); break;
          
        default:
          assert(false);
        }
      }

    private:
      Environment& env;
      
    private:
      void _int() {
        push(type::Int::make(readUint4()));
      }

      void _string() {
        uint4 length = readUint4();
        std::string str;
        env.getCodeStream().readString(str, length);
        push(type::String::make(str));
      }

      void _char() {
        push(type::Char::make(readUint4()));
      }
      
    private:
      opcode_t readOp() {
        return env.getCodeStream().readUint4();
      }
      
      uint4 readUint4() {
        return env.getCodeStream().readUint4();
      }

      void push(type::Object* x) {
        env.getDataStack().push(x);
      }
    };
  }
}

#endif

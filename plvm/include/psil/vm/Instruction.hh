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
        case   1: _int(); break; // => 最終的にはこの辺りは全部定数テーブルに移動した方が良いかも
        case   2: _string(); break;
        case   3: _char(); break;
        case   4: _symbol(); break;
        case   5: _nil(); break;
        case   6: _true(); break;
        case   7: _false(); break;
        case   8: _list(); break;
          
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

      void _symbol() {
        uint2 length = readUint2();
        std::string str;
        env.getCodeStream().readString(str, length);
        push(type::Symbol::make(str));
      }

      void _nil() {
        push(type::Nil::make());
      }

      void _true() {
        push(type::Boolean::make(true));
      }
      
      void _false() {
        push(type::Boolean::make(false));
      }
      
      void _list() {
        uint4 length = readUint4();
        type::Object* head = type::Nil::make();
        for(uint4 i=0; i < length; i++) {
          head = type::Cons::make(pop(), head);
        }
        push(head);
      }
      
    private:
      opcode_t readOp() {
        return env.getCodeStream().readUint4();
      }
      
      uint4 readUint4() {
        return env.getCodeStream().readUint4();
      }

      uint2 readUint2() {
        return env.getCodeStream().readUint2();
      }

      void push(type::Object* x) {
        env.getDataStack().push(x);
      }

      type::Object* pop() {
        return env.getDataStack().pop();
      }
    };
  }
}

#endif

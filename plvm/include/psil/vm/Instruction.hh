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
      static void execute(opcode_t op, Environment& env) {
        switch (op) {
        case   1: _int(env); break;
        case   2: _string(env); break;
          
        default:
          assert(false);
        }
      }

      static void _int(Environment& env) {
        uint4 n = env.getCodeStream().readUint4();
        env.getDataStack().push(type::Int::make(n));
      }

      static void _string(Environment& env) {
      }
      
    };
  }
}

#endif

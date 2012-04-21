#ifndef PSIL_VM_EXECUTOR_HH
#define PSIL_VM_EXECUTOR_HH

#include "Environment.hh"
#include "BytecodeObject.hh"

namespace psil {
  namespace vm {
    class Executor {
    public:
      Executor(Environment& env) : env(env) {
      }

      void execute(const BytecodeObject& bcobj) {
      }

      // for debug
      void printState() const {
      }

    private:
      Environment& env;
    };
  }
}

#endif

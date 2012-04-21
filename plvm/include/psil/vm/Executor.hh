#ifndef PSIL_VM_EXECUTOR_HH
#define PSIL_VM_EXECUTOR_HH

#include "Environment.hh"
#include "BytecodeObject.hh"

// TODO: stackはデータとリターンで分ける

namespace psil {
  namespace vm {
    class Executor {
    public:
      Executor(Environment& env) : env(env) {
      }

      bool execute(const BytecodeObject& bcobj) {
        return true;
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

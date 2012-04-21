#ifndef PSIL_VM_EXECUTOR_HH
#define PSIL_VM_EXECUTOR_HH

#include "Environment.hh"
#include "BytecodeObject.hh"
#include <string>

namespace psil {
  namespace vm {
    class Executor {
    public:
      Executor(Environment& env) : env(env) {}
      bool execute(const BytecodeObject& bcobj);

      std::string show() const;

    private:
      Environment& env;
    };
  }
}

#endif

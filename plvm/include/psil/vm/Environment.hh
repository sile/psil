#ifndef PSIL_VM_ENVIRONMENT_HH
#define PSIL_VM_ENVIRONMENT_HH

#include "BytecodeObject.hh"
#include "DataStack.hh"
#include "ReturnStack.hh"

namespace psil {
  namespace vm {
    class Environment {
    public:
      Environment() {}
      
    private:
      BytecodeObject* bcobj;

      DataStack dataStack;
      ReturnStack returnStack;
    };
  }
}

#endif

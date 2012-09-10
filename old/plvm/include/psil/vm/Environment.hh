#ifndef PSIL_VM_ENVIRONMENT_HH
#define PSIL_VM_ENVIRONMENT_HH

#include "BytecodeObject.hh"
#include "ByteStream.hh"
#include "ConstantTable.hh"
#include "DataStack.hh"
#include "ReturnStack.hh"
#include <string>

namespace psil {
  namespace vm {
    class Environment {
    public:
      Environment() {}
      void ready(BytecodeObject* bcobj) {
        this->bcobj = bcobj;
        dataStack.clear();
        returnStack.clear();
      }
      
      std::string show() const { return dataStack.show() + "\n" + returnStack.show(); }
      
      ByteStream& getCodeStream() { return bcobj->getCodeStream(); }
      const ConstantTable& getConstantTable() { return bcobj->constantTable(); }

      DataStack& getDataStack() { return dataStack; }
      ReturnStack& getReturnStack() { return returnStack; }
      BytecodeObject* getContext() { return bcobj; }

      void restoreContext(BytecodeObject* cxt, unsigned address) {
        bcobj = cxt;
        bcobj->getCodeStream().setPosition(address);
      }
      
    private:
      BytecodeObject* bcobj;

      DataStack dataStack;
      ReturnStack returnStack;
    };
  }
}

#endif

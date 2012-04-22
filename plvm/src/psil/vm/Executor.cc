#include "psil/vm/Executor.hh"
#include "psil/vm/Instruction.hh"

using namespace psil::vm;

bool Executor::execute(BytecodeObject& bcobj) {
  env.ready(&bcobj);
  Instruction ins(env);
  
  while(ins.hasNextOp()) {
    ins.execute();
  }
  
  return true;
}

std::string Executor::show() const {
  return "";
}

#include "psil/vm/Executor.hh"
#include "psil/vm/Instruction.hh"

using namespace psil::vm;

bool Executor::execute(BytecodeObject& bcobj) {
  env.ready(&bcobj);
  Instruction ins(env);
  
  while(ins.hasNextOp()) {
    ins.execute();
  }
  
  // TODO: 本当はここでpopした値を返すのが正しい
  return true;
}

std::string Executor::show() const {
  return "";
}

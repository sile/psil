#include "psil/vm/Executor.hh"
#include "psil/vm/Instruction.hh"

using namespace psil::vm;

bool Executor::execute(BytecodeObject& bcobj) {
  env.ready(&bcobj);
  for(;;) {
    ByteStream& in = env.getCodeStream();
    if(in.eos()) {
      break;
    }

    opcode_t op = in.readUint1();
    executeInstruction(op);
  }
  
  return true;
}

void Executor::executeInstruction(opcode_t op) {
  Instruction::execute(op, env);
}

std::string Executor::show() const {
  return "";
}

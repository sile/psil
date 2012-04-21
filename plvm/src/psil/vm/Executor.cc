#include "psil/vm/Executor.hh"

using namespace psil::vm;

bool Executor::execute(BytecodeObject& bcobj) {
  env.ready(&bcobj);
  return true;
}

std::string Executor::show() const {
  return "";
}


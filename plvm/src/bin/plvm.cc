#include "psil/vm/Environment.hh"
#include "psil/vm/Executor.hh"

int main() {
  psil::vm::Environment env;
  psil::vm::Executor exec(env);
  psil::vm::BytecodeObject bc;
  exec.execute(bc);
  return 0;
}

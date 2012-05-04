#ifndef PSIL_VM_NATIVE_HH
#define PSIL_VM_NATIVE_HH

#include "type.hh"
#include "Environment.hh"
#include "Object.hh"

namespace psil {
  namespace vm {
    using namespace type;
    
    class Native { // => Built In Function
      public:
      static void _i_add(Environment& env) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Int::make(x+y));
      }

      static void _i_sub(Environment& env) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Int::make(y-x));
      }

      static void _i_eql(Environment& env) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(x==y));
      }

      static void _i_less_than(Environment& env) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(y < x));
      }

      static void registerNatives() {
        reg("+", _i_add);
        reg("-", _i_sub);
        reg("=", _i_eql);
        reg("<", _i_less_than);
      }

    private:
      static Object* pop(Environment& env) { return env.getDataStack().pop(); }
      static int4 popInt(Environment& env) { return to<Int>(pop(env))->getValue(); }
      
      static void push(Environment& env, Object* x) {
        env.getDataStack().push(x);
      }
      
      static void reg(const char* name, NATIVE_FUN_T fn) {
        Symbol::make(name)->setValue(NativeLambda::make(fn));
      }
    };
  }
}

#endif

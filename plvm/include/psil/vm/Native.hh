#ifndef PSIL_VM_NATIVE_HH
#define PSIL_VM_NATIVE_HH

#include "type.hh"
#include "Environment.hh"
#include "Object.hh"
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

namespace psil {
  namespace vm {
    using namespace type;
    
    class Native { // => Built In Function
      public:
      static void _i_add(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Int::make(x+y));
      }

      static void _i_sub(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Int::make(y-x));
      }

      static void _i_eql(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(x==y));
      }

      static void _i_less_than(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(y < x));
      }

      static void _open_input_file(Environment& env, uint1 arity) {
        const std::string& path = popString(env);
        int fd = open(path.c_str(), O_RDONLY);
        if(! fd) {
          std::cerr << "Can't open input file: " << path << std::endl;
          assert(false);
        }
        push(env, Port::make(fd));
      }

      static void _open_output_file(Environment& env, uint1 arity) {
        const std::string& path = popString(env);
        int fd = open(path.c_str(), O_WRONLY | O_CREAT, 0755);
        if(fd == -1) {
          std::cerr << "Can't open input file(" << errno << "): " << path << std::endl;
          assert(false);
        }
        push(env, Port::make(fd));
      }
      
      static void _close_input_port(Environment& env, uint1 arity) {
        close(popPort(env));
        push(env, Undef::make());
      }

      static void _close_output_port(Environment& env, uint1 arity) {
        close(popPort(env));
        push(env, Undef::make());
      }

      static void _read_char(Environment& env, uint1 arity) {
        Port& p = (arity==0) ? Port::STDIN : *to<Port>(pop(env));
        if(p.hasBuffer()) {
          push(env, Char::make(p.getBufferedChar()));
          p.clearBuffer();
          return;
        }
        
        int fd = p.getValue();
        char ch = 0;
        int ret = read(fd, (void*)&ch, 1);

        if(ret == -1) {
          std::cerr << "read failed(" << errno << "): " << fd << std::endl;
          push(env, Undef::make());
        } else if (ret == 0){
          push(env, Symbol::make("EOF"));
        } else {
          push(env, Char::make(ch));
        }
      }

      static void _peek_char(Environment& env, uint1 arity) {
        Port& p = (arity==0) ? Port::STDIN : *to<Port>(pop(env));
        if(p.hasBuffer()) {
          push(env, Char::make(p.getBufferedChar()));
          return;
        }
        
        int fd = p.getValue();
        char ch = 0;
        int ret = read(fd, (void*)&ch, 1);

        if(ret == -1) {
          std::cerr << "read failed(" << errno << "): " << fd << std::endl;
          push(env, Undef::make());
        } else if (ret == 0){
          push(env, Symbol::make("EOF"));
        } else {
          p.setBufferedChar(ch);
          push(env, Char::make(ch));
        }
      }

      static void _eq(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env) == pop(env)));
      }

      static void registerNatives() {
        reg("EQ", _eq);
        reg("+", _i_add);
        reg("-", _i_sub);
        reg("=", _i_eql);
        reg("<", _i_less_than);
        reg("OPEN-INPUT-FILE", _open_input_file);
        reg("OPEN-OUTPUT-FILE", _open_output_file);
        reg("CLOSE-INPUT-PORT", _close_input_port);
        reg("CLOSE-OUTPUT-PORT", _close_output_port);
        reg("READ-CHAR", _read_char);
        reg("PEEK-CHAR", _peek_char);

        regval("STDIN", &Port::STDIN);
        regval("STDOUT", &Port::STDOUT);
        regval("STDERR", &Port::STDERR);
      }

    private:
      static Object* pop(Environment& env) { return env.getDataStack().pop(); }
      static int4 popInt(Environment& env) { return to<Int>(pop(env))->getValue(); }
      static int4 popPort(Environment& env) { return to<Port>(pop(env))->getValue(); }
      static const std::string& popString(Environment& env) { return to<String>(pop(env))->getValue(); }
      
      static void push(Environment& env, Object* x) {
        env.getDataStack().push(x);
      }
      
      static void reg(const char* name, NATIVE_FUN_T fn) {
        Symbol::make(name)->setValue(NativeLambda::make(fn));
      }
      static void regval(const char* name, Object* val) {
        Symbol::make(name)->setValue(val);
      }
    };
  }
}

#endif

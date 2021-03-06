#ifndef PSIL_VM_NATIVE_HH
#define PSIL_VM_NATIVE_HH

#include "type.hh"
#include "Environment.hh"
#include "Object.hh"
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/select.h>

namespace psil {
  namespace vm {
    using namespace type;
    
    class Native { // => Built In Function
      public:
      static void _eq(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env) == pop(env)));
      }

      static void _is_undef(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env) == Undef::make()));
      }

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

      static void _i_mul(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Int::make(y*x));
      }

      static void _i_div(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Int::make(y/x));
      }

      static void _i_mod(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Int::make(y % x));
      }

      static void _i_eql(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(x==y));
      }

      static void _i_not_eql(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(x!=y));
      }

      static void _i_less_than(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(y < x));
      }

      static void _i_less_eql_than(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(y <= x));
      }

      static void _i_greater_than(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(y > x));
      }

      static void _i_greater_eql_than(Environment& env, uint1 arity) {
        int4 x = popInt(env);
        int4 y = popInt(env);
        push(env, Boolean::make(y >= x));
      }

      static void _open_input_file(Environment& env, uint1 arity) {
        const std::string& path = popString(env);
        int fd = open(path.c_str(), O_RDONLY);
        if(! fd) {
          std::cerr << "Can't open input file: " << path << std::endl;
          assert(false);
        }
        push(env, Port::make(fd, true));
      }

      static void _open_output_file(Environment& env, uint1 arity) {
        const std::string& path = popString(env);
        int fd = open(path.c_str(), O_WRONLY | O_CREAT, 0755);
        if(fd == -1) {
          std::cerr << "Can't open input file(" << errno << "): " << path << std::endl;
          assert(false);
        }
        push(env, Port::make(fd, false));
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
        Port& p = *to<Port>(arity==0 ? Symbol::make("CURRENT-INPUT")->getValue() : pop(env));
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
          push(env, Port::END_OF_FILE);
        } else {
          push(env, Char::make(ch));
        }
      }

      static void _write_char(Environment& env, uint1 arity) {
        Port& p = *to<Port>(arity==1 ? Symbol::make("CURRENT-OUTPUT")->getValue() : pop(env));
        char ch = (char)to<Char>(pop(env))->getCode(); 
        
        int fd = p.getValue();
        int ret = write(fd, (void*)&ch, 1);

        if(ret == -1) {
          std::cerr << "write failed(" << errno << "): " << fd << std::endl;
        } 
        push(env, Undef::make());
      }

      static void _write_byte(Environment& env, uint1 arity) {
        Port& p = *to<Port>(arity==1 ? Symbol::make("CURRENT-OUTPUT")->getValue() : pop(env));
        char ch = (char)popInt(env);
        
        int fd = p.getValue();
        int ret = write(fd, (void*)&ch, 1);

        if(ret == -1) {
          std::cerr << "write failed(" << errno << "): " << fd << std::endl;
        } 
        push(env, Undef::make());
      }
      
      static void _peek_char(Environment& env, uint1 arity) {
        Port& p = *to<Port>(arity==0 ? Symbol::make("CURRENT-INPUT")->getValue() : pop(env));
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
          push(env, Port::END_OF_FILE);
        } else {
          p.setBufferedChar(ch);
          push(env, Char::make(ch));
        }
      }
      
      static void _is_eof_object(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env) == Port::END_OF_FILE));
      }

      static void _is_char_ready(Environment& env, uint1 arity) {
        Port& p = *to<Port>(arity==0 ? Symbol::make("CURRENT-INPUT")->getValue() : pop(env));
        if(p.hasBuffer()) {
          push(env, Boolean::make(true));
          return;
        }
        
        int fd = p.getValue();

        int flag = fcntl(fd, F_GETFL, 0);
        fcntl(fd, F_SETFL, O_NONBLOCK|flag);
        char ch = 0;
        int ret = read(fd, (void*)&ch, 1);
        if(ret == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
          push(env, Boolean::make(false));
        } else if (ret == -1) {
          std::cerr << "read failed(" << errno << "): " << fd << std::endl;
          push(env, Undef::make());
        } else {
          if(ret != 0) {
            p.setBufferedChar(ch);
          }
          push(env, Boolean::make(true));
        }
        fcntl(fd, F_SETFL, flag);
      }

      static void _is_input_port(Environment& env, uint1 arity) {
        push(env, Boolean::make(to<Port>(pop(env))->isInputPort()));
      }

      static void _is_output_port(Environment& env, uint1 arity) {
        push(env, Boolean::make(to<Port>(pop(env))->isInputPort() == false));
      }
      
      static void _current_input_port(Environment& env, uint1 arity) {
        push(env, Symbol::make("CURRENT-INPUT")->getValue());
      }

      static void _current_output_port(Environment& env, uint1 arity) {
        push(env, Symbol::make("CURRENT-OUTPUT")->getValue());
      }

      static void _is_pair(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_CONS));
      }

      static void _is_null(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_NIL));
      }
      
      static void _car(Environment& env, uint1 arity) {
        push(env, to<Cons>(pop(env))->getCar());
      }

      static void _cdr(Environment& env, uint1 arity) {
        push(env, to<Cons>(pop(env))->getCdr());
      }

      static void _cons(Environment& env, uint1 arity) {
        Object* cdr = pop(env);
        Object* car = pop(env);
        push(env, Cons::make(car, cdr));
      }

      static void _set_car(Environment& env, uint1 arity) {
        Object* val = pop(env);
        Cons* cons = to<Cons>(pop(env));
        cons->setCar(val);
        push(env, Undef::make());
      }

      static void _set_cdr(Environment& env, uint1 arity) {
        Object* val = pop(env);
        Cons* cons = to<Cons>(pop(env));
        cons->setCdr(val);
        push(env, Undef::make());
      }

      static void _integer_to_char(Environment& env, uint1 arity) {
        push(env, Char::make(to<Int>(pop(env))->getValue()));
      }

      static void _char_to_integer(Environment& env, uint1 arity) {
        push(env, Int::make(to<Char>(pop(env))->getCode()));
      }

      static void _char_eql(Environment& env, uint1 arity) {
        Char& y = *to<Char>(pop(env));
        Char& x = *to<Char>(pop(env));
        push(env, Boolean::make(x.getCode() == y.getCode()));
      }

      static void _char_less_eql_than(Environment& env, uint1 arity) {
        Char& y = *to<Char>(pop(env));
        Char& x = *to<Char>(pop(env));
        push(env, Boolean::make(x.getCode() <= y.getCode()));        
      }

      static void _char_upcase(Environment& env, uint1 arity) {
        push(env, Char::make(toupper(to<Char>(pop(env))->getCode())));
      }

      static void _char_downcase(Environment& env, uint1 arity) {
        push(env, Char::make(tolower(to<Char>(pop(env))->getCode())));
      }

      static void _list(Environment& env, uint1 arity) {
        Object* head = Nil::make();
        for(uint1 i=0; i < arity; i++) {
          head = Cons::make(pop(env), head);
        }
        push(env, head);
      }

      static void _undef(Environment& env, uint1 arity) {
        push(env, Undef::make());
      }

      static void _is_eqv(Environment& env, uint1 arity) {
        Object* x = pop(env);
        Object* y = pop(env);
        if(x == y) {
          // boolean, symbol
          push(env, Boolean::make(true));
          return;
        }

        if(x->getType() != y->getType()) {
          push(env, Boolean::make(false));
          return;
        }

        if(x->getType() == TYPE_INT && to<Int>(x)->getValue() == to<Int>(y)->getValue()) {
          // integer
          push(env, Boolean::make(true));
          return;
        }
        
        if(x->getType() == TYPE_CHAR && to<Char>(x)->getCode() == to<Char>(y)->getCode()) {
          // character
          push(env, Boolean::make(true));
          return;
        }
        
        push(env, Boolean::make(false));
      }

      static void _make_string(Environment& env, uint1 arity) {
        char ch = arity == 1 ? 0 : (char)to<Char>(pop(env))->getCode();
        uint4 len = popInt(env);
        std::string s;
        s.resize(len);
        for(uint4 i=0; i < len; i++) {
          s[i] = ch;
        }
        push(env, String::make(s));
      }

      static void _string_ref(Environment& env, uint1 arity) {
        uint4 index = popInt(env);
        std::string& s = to<String>(pop(env))->getValue();
        push(env, Char::make(s[index]));
      }

      static void _string_set(Environment& env, uint1 arity) {
        char ch = (char)to<Char>(pop(env))->getCode();
        uint4 index = popInt(env);
        std::string& s = to<String>(pop(env))->getValue();
        s[index] = ch;
        push(env, Undef::make());
      }

      static void _string_to_symbol(Environment& env, uint1 arity) {
        std::string& s = to<String>(pop(env))->getValue();
        push(env, Symbol::make(s));
      }

      static void _string_length(Environment& env, uint1 arity) {
        std::string& s = to<String>(pop(env))->getValue();
        push(env, Int::make(s.size()));
      }

      static void _is_boolean(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_BOOL));
      }

      static void _is_symbol(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_SYMBOL));
      }

      static void _is_char(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_CHAR));
      }

      static void _is_number(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_INT));
      }

      static void _is_string(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_STRING));
      }
      
      /*
      static void _is_vector(Environment& env, uint1 arity) {
      }
      */

      static void _is_port(Environment& env, uint1 arity) {
        push(env, Boolean::make(pop(env)->getType() == TYPE_PORT));
      }

      static void _is_procedure(Environment& env, uint1 arity) {
        Object* o = pop(env);
        push(env, Boolean::make(o->getType() == TYPE_LAMBDA || o->getType() == TYPE_NATIVE_LAMBDA));
      }

      static void _is_list(Environment& env, uint1 arity) {
        Object* o = pop(env);        
        push(env, Boolean::make(o->getType() == TYPE_NIL || o->getType() == TYPE_CONS));
      }

      static void _bit_field(Environment& env, uint1 arity) {
        uint4 end = popInt(env);
        uint4 start = popInt(env);
        uint4 n = popInt(env);
        uint4 n2 = (n & (((1 << end)-1) - ((1 << start) -1))) >> start;
        push(env, Int::make(n2));
      }

      static void _symbol_to_string(Environment& env, uint1 arity) {
        push(env, String::make(to<Symbol>(pop(env))->getName()));
      }

      static void _symbol_value(Environment& env, uint1 arity) {
        push(env, to<Symbol>(pop(env))->getValue());
      }

      static void registerNatives() {
        reg("EQ", _eq);
        reg("EQ?", _eq);
        reg("EQV?", _is_eqv);
        reg("UNDEF?", _is_undef);
        reg("BOOLEAN?", _is_boolean);
        reg("SYMBOL?", _is_symbol);
        reg("CHAR?", _is_char);
        reg("NUMBER?", _is_number);
        reg("STRING?", _is_string);
        // reg("VECTOR?", _is_vector);
        reg("PORT?", _is_port);
        reg("PROCEDURE?", _is_procedure);
        reg("LIST?", _is_list);
        reg("+", _i_add);
        reg("-", _i_sub);
        reg("*", _i_mul);
        reg("/", _i_div);
        reg("MODULO", _i_mod);
        reg("=", _i_eql);
        reg("/=", _i_not_eql);
        reg("<", _i_less_than);
        reg("<=", _i_less_eql_than);
        reg(">=", _i_greater_eql_than);
        reg(">", _i_greater_than);
        reg("BIT-FIELD", _bit_field);
        reg("OPEN-INPUT-FILE", _open_input_file);
        reg("OPEN-OUTPUT-FILE", _open_output_file);
        reg("CLOSE-INPUT-PORT", _close_input_port);
        reg("CLOSE-OUTPUT-PORT", _close_output_port);
        reg("PEEK-CHAR", _peek_char);
        reg("READ-CHAR", _read_char);
        reg("WRITE-CHAR", _write_char);
        reg("WRITE-BYTE", _write_byte);
        reg("CHAR-READY?", _is_char_ready);
        reg("INPUT-PORT?", _is_input_port);
        reg("OUTPUT-PORT?", _is_output_port);
        reg("CURRENT-INPUT-PORT", _current_input_port);
        reg("CURRENT-OUTPUT-PORT", _current_output_port);
        reg("EOF-OBJECT?", _is_eof_object);
        reg("PAIR?", _is_pair);
        reg("NULL?", _is_null);
        reg("CAR", _car);
        reg("CDR", _cdr);
        reg("CONS", _cons);
        reg("SET-CAR!", _set_car);
        reg("SET-CDR!", _set_cdr);
        reg("INTEGER->CHAR", _integer_to_char);
        reg("CHAR->INTEGER", _char_to_integer);
        reg("CHAR-UPCASE", _char_upcase);
        reg("CHAR-DOWNCASE", _char_downcase);
        reg("CHAR=", _char_eql);
        reg("CHAR<=", _char_less_eql_than);
        reg("LIST", _list);
        reg("MAKE-STRING", _make_string);
        reg("STRING-REF", _string_ref);
        reg("STRING-SET!", _string_set);
        reg("STRING->SYMBOL", _string_to_symbol);
        reg("STRING-LENGTH", _string_length);
        reg("UNDEF", _undef);
        reg("SYMBOL->STRING", _symbol_to_string);
        reg("SYMBOL-VALUE", _symbol_value);

        regval("STDIN", &Port::STDIN);
        regval("STDOUT", &Port::STDOUT);
        regval("STDERR", &Port::STDERR);
        regval("CURRENT-INPUT", Port::CURRENT_INPUT);
        regval("CURRENT-OUTPUT", Port::CURRENT_OUTPUT);
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

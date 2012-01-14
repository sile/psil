#ifndef PSIL_CORE_INTERPRETER_HH
#define PSIL_CORE_INTERPRETER_HH

#include "util.hh"

#include "bytecode.hh"
#include "bytecode_reader.hh"
#include "symbol_table.hh"
#include "object.hh"
#include "bindings.hh"
#include "environment.hh"
#include "native.hh"

#include <iostream>
#include <string>
#include <cassert>

namespace psil {
  namespace core {
    class interpreter {
      typedef bytecode_reader reader;
      
    public:
      interpreter() {
        std::cout << "[INITIALIZE]" << std::endl;
        std::cout << "# native function:" << std::endl;
        
        NATIVE_FN natives[] =
          {
            native::i_plus, native::i_minus, native::i_mul, native::i_div,
            native::i_eql, native::i_less,
            native::car, native::cdr, native::cons
          };
        
        for(unsigned i=0; i < sizeof(natives)/sizeof(NATIVE_FN); i++) {
          std::cout << " # [" << i << "] " << (long)natives[i] << std::endl;
          env.add_native(i, natives[i]);
        }

        std::cout << std::endl;
      }
      
      void interpret(std::istream& in) {
        bytecode_reader in2(in);
        interpret(in2);
      }
      void interpret(reader& in) {
        std::cout << "[INTERPRET]" << std::endl;

        // header
        bytecode::header hdr;
        in.read_header(hdr);
        assert(hdr.version == 1);
        std::cout << "# version: " << hdr.version << std::endl;
        
        // symbol-table
        in.read_symbol_table(hdr, symbols);
        
        std::cout << "# symbol: " << std::endl;
        for(int i=0; i < symbols.size(); i++) {
          std::cout << " # [" << symbols.get_entry(i).sym->value() << "]" 
                    << " " << symbols.get_entry(i).name->show(buf) << std::endl;
        }

        // initial-data
        in.read_init_data(hdr, env.get_binds(), symbols);

        std::cout << "# data: " << std::endl;
        {
          bindings::const_iterator itr = env.get_binds().begin();
          for(; itr != env.get_binds().end(); ++itr) {
            std::cout << " # [" << itr->first << "] " << itr->second->show(buf) <<  std::endl;
          }
        }
        
        // interpret
        std::cout << "# interpret:" << std::endl;
        do_interpret(in);
      }

    private:
      void do_interpret(reader& in) { 
        obj::object* o = in.read_object(symbols);
        std::cout << " # read: " << o->show(buf) << std::endl;
        
        obj::object* result = eval_expression(o, env);
        std::cout << " # result: " << result->show(buf) << std::endl;
      }

      obj::object* symbol_value(obj::symbol* sym, environment& e) {
        return e.symbol_value(sym);
      }

      obj::object* eval_expression(obj::object* o, environment& e) {
        obj::object* result;
        switch(o->type()) {
        case obj::O_LIST: 
          o = ((obj::list*)o)->value();
          if(o == &obj::NIL) {
            result = o;
            break;
          }
        case obj::O_CONS: 
          if(is_proper_list((obj::cons*)o)==false)
            ERR(o->show(buf)+" is not proper list");
          result = eval_cons((obj::cons*)o, e);
          break;
        
        case obj::O_REFER: 
          // TODO?
          break;

        case obj::O_SYMBOL: 
          result = symbol_value(reinterpret_cast<obj::symbol*>(o), e);
          break;
          
        case obj::O_QUOTE:
          result = ((obj::quote*)o)->value();
          break;
          
        case obj::O_OBJECT:          
        case obj::O_INTEGER:
        case obj::O_STRING: 
        case obj::O_FUNCTION:
        case obj::O_MACRO_FUNCTION:
        case obj::O_NATIVE_FUNCTION:
        case obj::O_SPECIAL:
          result = o;
          break;
        }
        return result;
      }

      obj::object* eval_cons(obj::cons* o, environment& e) {
        obj::object* car = eval_expression(o->get_car(), e);
        obj::list* args = obj::lists::to_list(o->get_cdr());

        switch(car->type()) {
        case obj::O_SPECIAL:
          return eval_special_form((obj::special*)car, args, e);
          
        case obj::O_FUNCTION:
          return eval_function((obj::function*)car, args, e);

        case obj::O_MACRO_FUNCTION:
          return eval_macro_function((obj::macro_function*)car, args, e);

        case obj::O_NATIVE_FUNCTION:
          return eval_native_function((obj::native_function*)car, args, e);
          
        case obj::O_QUOTE:
        case obj::O_SYMBOL: 
        case obj::O_REFER: 
        case obj::O_CONS:           
        case obj::O_LIST:
        case obj::O_OBJECT:          
        case obj::O_INTEGER:
        case obj::O_STRING: 
          ERR(o->show(buf)+" is not a function");
        }
        return o;
      }

      obj::object* eval_special_form(obj::special* sf, obj::list* args, environment& e) {
        switch(sf->value()) {
        case obj::special::LAMBDA:
          return eval_sf_lambda(args ,e);

        case obj::special::PROGN:
          return eval_sf_progn(args, e);

        case obj::special::IF:
          return eval_sf_if(args, e);
          
        case obj::special::LAMBDA_MACRO:
          return eval_sf_lambda_macro(args, e);
          
        default:
          ERR(sf->value()+" is not a special form");
        }
      }

      obj::object* eval_sf_if(obj::list* args, environment& e) {
        assert(args->is_null()==false);
        obj::object* cond = obj::lists::car(args);
        
        args = obj::lists::cdr_list(args);
        assert(args->is_null()==false);
        obj::object* then = obj::lists::car(args);
        
        args = obj::lists::cdr_list(args);
        obj::object* alt = obj::lists::car(args); // optional
        
        if(obj::is_nil(eval_expression(cond, e)))
          return eval_expression(alt, e);
        return eval_expression(then, e);
      }
      
      obj::object* eval_sf_lambda(obj::list* args, environment& e) {
        assert(args->is_null()==false);
        return new obj::function(obj::lists::to_list(obj::lists::car(args)),
                                 obj::lists::cdr_list(args));
      }

      obj::object* eval_sf_lambda_macro(obj::list* args, environment& e) {
        assert(args->is_null()==false);
        return new obj::macro_function(obj::lists::to_list(obj::lists::car(args)),
                                       obj::lists::cdr_list(args));
      }
      
      obj::object* eval_sf_progn(obj::list* args, environment& e) {
        obj::object* result = obj::o_nil();

        X_LIST_EACH(a, args, {
            result = eval_expression(a, e);
          });

        return result;
      }
      
      obj::object* eval_function(obj::function* fn, obj::list* args, environment& e) {
        environment& fn_e = *e.in_scope();
        fn_e.bind_symbols(fn->get_params(), args);
        return eval_expression(fn->get_body()->value(), fn_e);
      }

      obj::object* eval_macro_function(obj::macro_function* fn, obj::list* args, environment& e) {
        return eval_expression(eval_function(fn, args, e), e);
      }

      obj::object* eval_native_function(obj::native_function* fn, obj::list* args, environment& e) {
        return fn->apply(environment::native_fun_table, args, &e);
      }
      
      bool is_proper_list(obj::cons* cons) {
        // TODO:
        return true;
      }
    private:
      std::string buf;
      
      symbol_table symbols;
      environment env; // global
    };
  }
}

#endif

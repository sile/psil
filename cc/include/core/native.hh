#ifndef PSIL_CORE_NATIVE_HH
#define PSIL_CORE_NATIVE_HH

#include "util.hh"
#include "object.hh"
#include "environment.hh"
#include <cassert>

// XXX: 
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

namespace psil {
  namespace core {
    namespace native {
      namespace {
        obj::object* succeeded(int ret) {
          return ret==-1 ? obj::o_nil() : obj::o_t();
        }
      }

      // +, -, *, /
      obj::object* i_plus(obj::list* args, environment* env) {
        int sum = 0;
        X_LIST_EACH(n, args, {
            sum += obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }

      obj::object* i_minus(obj::list* args, environment* env) {
        assert(args->length() > 0);

        int sum = obj::to_integer(obj::lists::car(args))->value();
        X_LIST_EACH(n, obj::lists::cdr_list(args), {
            sum -= obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }

      obj::object* i_mul(obj::list* args, environment* env) {
        int sum = 1;
        X_LIST_EACH(n, args, {
            sum *= obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }   

      obj::object* i_div(obj::list* args, environment* env) {
        assert(args->length() > 0);

        int sum = obj::to_integer(obj::lists::car(args))->value();
        X_LIST_EACH(n, obj::lists::cdr_list(args), {
            sum /= obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }  

      // =, <
      obj::object* i_eql(obj::list* args, environment* env) {
        assert(args->length() > 0);
        
        int a = obj::to_integer(obj::lists::car(args))->value();
        X_LIST_EACH(n, obj::lists::cdr_list(args), {
            if(a != obj::to_integer(n)->value())
              return obj::o_nil();
        });
        
        return obj::o_t();
      } 

      obj::object* i_less(obj::list* args, environment* env) {
        assert(args->length() > 0);
        
        int a = obj::to_integer(obj::lists::car(args))->value();
        X_LIST_EACH(n, obj::lists::cdr_list(args), {
            if(!(a < obj::to_integer(n)->value())) 
              return obj::o_nil();
            a = obj::to_integer(n)->value();
        });
        
        return obj::o_t();
      }

      // car, cdr, cons, list
      obj::object* car(obj::list* args, environment* env) {
        assert(args->length() == 1);
        
        return obj::lists::car(obj::lists::to_list(obj::lists::car(args)));
      }

      obj::object* cdr(obj::list* args, environment* env) {
        assert(args->length() == 1);
        
        return obj::lists::cdr(obj::lists::to_list(obj::lists::car(args)));
      }

      obj::object* cons(obj::list* args, environment* env) {
        assert(args->length() == 2);
        
        return new obj::cons(obj::lists::car(args),
                             obj::lists::car(obj::lists::cdr_list(args)));
      }

      obj::object* list(obj::list* args, environment* env) {
        return args->value();
      }

      // set-symbol-value
      obj::object* set_symbol_value(obj::list* args, environment* env) {
        assert(args->length() == 2);
        
        obj::object* fst = obj::lists::car(args);
        obj::object* snd = obj::lists::car(obj::lists::cdr_list(args));        
        
        obj::symbol* sym = obj::to_symbol(fst);
        
        environment* g_env = env->get_global_env();
        g_env->bind_symbol(sym, snd);
        return sym;
      }

      // open, close, read_byte, write_byte
      obj::object* open(obj::list* args, environment* env) {
        assert(args->length() == 2);
        
        obj::string* path = obj::to_string(obj::lists::first(args));
        obj::integer* flags = obj::to_integer(obj::lists::second(args));

        std::string buf;
        
        
        int ret = ::open(path->c_string(buf).c_str(), flags->value());
        if(ret==-1)
          return obj::o_nil();
        return new obj::stream(ret);
      }

      obj::object* close(obj::list* args, environment* env) {
        assert(args->length() == 1);
        obj::stream* stream = obj::to_stream(obj::lists::first(args));

        int ret = ::close(stream->value());
        if(ret==-1)
          return obj::o_nil();
        return obj::o_t();
      }
      
      obj::object* read_byte(obj::list* args, environment* env) {
        assert(args->length() == 0 || args->length() == 1);
        
        obj::object* fst = obj::lists::car(args);
        int fd = obj::is_nil(fst) ? 1 : obj::to_stream(fst)->value();
        int buf;
        if(read(fd, &buf, 1)==-1)
          return obj::o_nil();
        return new obj::integer(buf&0xFF);
      }

      obj::object* write_byte(obj::list* args, environment* env) {
        assert(args->length() == 1 || args->length() == 2);
        
        obj::object* fst = obj::lists::car(args);
        obj::object* snd = obj::lists::car(obj::lists::cdr_list(args));
      
        char byte = obj::to_integer(fst)->value() & 0xFF;
        int fd = obj::is_nil(snd) ? 1 : obj::to_stream(snd)->value();
        return succeeded(write(fd, &byte, 1));
      }

      // eq
      obj::object* eq(obj::list* args, environment* env) {
        assert(args->length() == 2);
       
        if(obj::lists::first(args) == obj::lists::second(args))
          return obj::o_t();
        return obj::o_nil();
      }

      // list-to-string
      obj::object* list_to_string(obj::list* args, environment* env) {
        assert(args->length() == 1);
        return new obj::string(obj::lists::to_list(obj::lists::first(args)));
      }
    }
  }
}

#endif

#ifndef PSIL_CORE_NATIVE_HH
#define PSIL_CORE_NATIVE_HH

#include "util.hh"
#include "object.hh"
#include "environment.hh"
#include <cassert>

// XXX: 
#include <unistd.h>

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

      // car, cdr, cons
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

      // open, close, read_byte, write_byte
      obj::object* read_byte(obj::list* args, environment* env) {
        assert(args->length() == 0 || args->length() == 1);
        
        obj::object* fst = obj::lists::car(args);
        int fd = obj::is_nil(fst) ? 1 : obj::to_integer(fst)->value();
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
        int fd = obj::is_nil(snd) ? 1 : obj::to_integer(snd)->value();
        return succeeded(write(fd, &byte, 1));
      }
    }
  }
}

#endif

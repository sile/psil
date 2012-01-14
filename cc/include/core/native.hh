#ifndef PSIL_CORE_NATIVE_HH
#define PSIL_CORE_NATIVE_HH

#include "util.hh"
#include "object.hh"
#include "environment.hh"
#include <cassert>

namespace psil {
  namespace core {
    namespace native {
      obj::object* i_plus(obj::list* args, environment* env) {
        int sum = 0;
        LIST_EACH(n, args, {
            sum += obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }

      obj::object* i_minus(obj::list* args, environment* env) {
        assert(args->length() > 0);

        int sum = obj::to_integer(obj::list::car(args))->value();
        LIST_EACH(n, obj::list::cdr_list(args), {
            sum -= obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }

      obj::object* i_mul(obj::list* args, environment* env) {
        int sum = 1;
        LIST_EACH(n, args, {
            sum *= obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }   

      obj::object* i_div(obj::list* args, environment* env) {
        assert(args->length() > 0);

        int sum = obj::to_integer(obj::list::car(args))->value();
        LIST_EACH(n, obj::list::cdr_list(args), {
            sum /= obj::to_integer(n)->value();
        });
        return new obj::integer(sum);
      }   
    }
  }
}

#endif

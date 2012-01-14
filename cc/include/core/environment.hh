#ifndef PSIL_CORE_ENVIRONMENT_HH
#define PSIL_CORE_ENVIRONMENT_HH

#include "bindings.hh"
#include "object.hh"
#include "util.hh"
#include <string>
#include <cassert>

namespace psil {
  namespace core {
    class environment {
    public:
      environment() : parent(NULL) {}
      environment(environment* parent) : parent(parent) {}

      bindings& get_binds() { return binds; }
      
      obj::object* symbol_value(obj::symbol* sym) {
        if(binds.find(sym->value()) != binds.end())
          return binds[sym->value()];
        if(parent==NULL)
          ERR(std::string("unbinded symbol: ")+sym->show(buf));
        
        return parent->symbol_value(sym);
      }

      environment* in_scope() {
        return new environment(this);
      }

      environment* out_scope() {
        return parent;
      }

      void bind_symbols(obj::list* symbols, obj::list* values) {
        assert(symbols->length() == values->length());

        X_LIST_EACH2(sym, val, symbols, values, {
            binds[((obj::symbol*)sym)->value()] = val;
        });
      }
      
      static void add_native(int index, NATIVE_FN fn) {
        native_fun_table[index] = fn;
      }
      
    public:
      static NATIVE_FN native_fun_table[256]; // XXX:

    private:
      std::string buf;
      bindings binds;
      environment* parent;
    };

    NATIVE_FN environment::native_fun_table[256] = {NULL};
  }
}

#endif

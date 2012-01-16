#ifndef PSIL_CORE_ENVIRONMENT_HH
#define PSIL_CORE_ENVIRONMENT_HH

#include "symbol_table.hh"
#include "bindings.hh"
#include "object.hh"
#include "util.hh"
#include <string>
#include <cassert>

namespace psil {
  namespace core {
    class environment {
    public:
      environment(symbol_table* symbols) : symbols(symbols), parent(NULL) {}
      environment(symbol_table* symbols, environment* parent) : symbols(symbols), parent(parent) {}

      bindings& get_binds() { return binds; }
      
      obj::object* symbol_value(obj::symbol* sym) {
        if(binds.find(sym->value()) != binds.end())
          return binds[sym->value()];
        if(parent==NULL)
          ERR(std::string("unbinded symbol: ")+sym->show(buf));
        
        return parent->symbol_value(sym);
      }

      environment* in_scope() {
        return new environment(symbols, this);
      }

      environment* out_scope() {
        return parent;
      }

      void bind_symbols(obj::list* symbols, obj::list* values) {
        obj::symbol* rest = obj::symbol::intern2("&REST");

        obj::list* sl = symbols;
        obj::list* vl = values;
        
        std::string b;
        for(; sl->is_null()==false;
            sl = obj::lists::cdr_list(sl), vl = obj::lists::cdr_list(vl)) {
          obj::symbol* s = obj::to_symbol(obj::lists::car(sl));

          if(s->eq(rest)) {
            sl = obj::lists::cdr_list(sl);
            s = obj::to_symbol(obj::lists::car(sl));
            assert(obj::lists::cdr_list(sl)->is_null());
            
            binds[s->value()] = vl->value();
            return;
          } else {
            if(vl->is_null())
              break;
            obj::object* v = obj::lists::car(vl);
            binds[s->value()] = v;
          }
        }
        assert(sl->is_null() && vl->is_null());
      }
      
      static void add_native(int index, NATIVE_FN fn) {
        native_fun_table[index] = fn;
      }
      
      void bind_symbol(obj::symbol* symbol, obj::object* value) {
        binds[symbol->value()] = value;
      }

      environment* get_global_env() { 
        if(parent == NULL)
          return this;
        return parent->get_global_env();
      }

    public:
      static NATIVE_FN native_fun_table[256]; // XXX:

    private:
      std::string buf;
      bindings binds;
      symbol_table *symbols;
      environment* parent;
    };

    NATIVE_FN environment::native_fun_table[256] = {NULL};
  }
}

#endif

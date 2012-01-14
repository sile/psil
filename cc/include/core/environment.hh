#ifndef PSIL_CORE_ENVIRONMENT_HH
#define PSIL_CORE_ENVIRONMENT_HH

#include "bindings.hh"
#include "object.hh"
#include <string>

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

    private:
      std::string buf;
      bindings binds;
      environment* parent;
    };
  }
}

#endif

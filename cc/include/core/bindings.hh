#ifndef PSIL_CORE_BINDINGS
#define PSIL_CORE_BINDINGS

#include "object.hh"
#include <tr1/unordered_map>

namespace psil {
  namespace core {
    class bindings : public std::tr1::unordered_map<int, obj::object*> {
      
    };
  }
}

#endif

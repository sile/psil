#ifndef PSIL_CORE_ENVIRONMENT_HH
#define PSIL_CORE_ENVIRONMENT_HH

#include "bindings.hh"

namespace psil {
  namespace core {
    class environment {
    public:
      bindings& get_binds() { return binds; }

    private:
      bindings binds;
    };
  }
}

#endif

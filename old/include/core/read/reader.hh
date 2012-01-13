#ifndef PSIL_CORE_READ_READER_HH
#define PSIL_CORE_READ_READER_HH

#include "../object/object.hh"

namespace psil {
namespace core {
namespace read {
  class Reader {
  public:
    const object::Object* read(const char* start, const char* end);
  };
}
}
}

#endif

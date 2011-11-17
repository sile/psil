#ifndef PSIL_CORE_UTIL_CHAR_STREAM_HH
#define PSIL_CORE_UTIL_CHAR_STREAM_HH

#include <cstring>

namespace psil {
namespace core {
namespace util {
  class CharStream {
  public:
    CharStream(const char* source) { CharStream(source, source+strlen(source)); }
    CharStream(const char* start, const char* end) : cur(start), end(end) {}
  private:
    const char* cur;
    const char* end;
  };
}
}
}

#endif

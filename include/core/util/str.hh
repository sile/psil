#ifndef PSIL_CORE_UTIL_STR_HH
#define PSIL_CORE_UTIL_STR_HH

#include <sstream>
#include <string>

namespace psil {
namespace core {
namespace util {
  class Str {
  public:
    template <typename T>
    static std::string toString(const T& x) {
      std::stringstream ss;
      ss << x;
      return ss.str();
    }
  };
}
}
}

#endif

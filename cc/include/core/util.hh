#ifndef PSIL_CORE_UTIL_HH
#define PSIL_CORE_UTIL_HH

#include <string>
#include <sstream>
#include <iostream>

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

      void err(const std::string& message) {
        std::cerr << std::endl << "[ERROR] " << message << std::endl;
      }

      template <typename T>
      std::string to_string(const T& x) {
        return Str::toString(x);
      }
    }
  }
}

#define ERR(message) {psil::core::util::err(message); throw "<<abort>>";}

#endif

#ifndef PSIL_CORE_UTIL_CHAR_STREAM_HH
#define PSIL_CORE_UTIL_CHAR_STREAM_HH

#include <cstring>

namespace psil {
namespace core {
namespace util {
  class CharStream {
  public:
    typedef const char* Mark;

  public:
    CharStream(const char* source) { CharStream(source, source+strlen(source)); }
    CharStream(const char* start, const char* end) : cur(start), end(end) {}

    char peek() const {
      return *cur;
    }

    char read() {
      if(is_eos()) 
        return '\n';

      char c = peek();
      cur++;
      return c;
    }

    bool is_eos() const {
      return cur >= end;
    }

    Mark position() const { return cur; }
    void position(Mark pos) { cur = pos; }

  private:
    const char* cur;
    const char* end;
  };
}
}
}

#endif

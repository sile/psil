#ifndef PSIL_VM_BYTE_STREAM_HH
#define PSIL_VM_BYTE_STREAM_HH

#include "type.hh"
#include <cstring>
#include <cassert>
#include <string>

namespace psil {
  namespace vm {
    class ByteStream {
    public:
      ByteStream(const char* bytes, unsigned size) : bytes(bytes), size(size), cur(0) {}
      bool eos() const { return cur >= size; }

      // TODO: endian
      uint1 readUint1() { assert(! eos()); return (uint1)bytes[cur++]; }
      uint2 readUint2() { return (uint2)(readUint1() << 8 | readUint1()); }
      uint4 readUint4() { return (uint4)(readUint2() << 16 | readUint2()); }
      void readBytes(char* buffer, unsigned size) {
        assert(cur + size <= this->size);
        memcpy(buffer, bytes, size);
        cur += size;
      }
      void readString(std::string& buffer, unsigned length) {
        assert(cur + length <= size);
        buffer.assign(bytes + cur, length);
        cur += length;
      }

      unsigned getPosition() const { return cur; }
      ByteStream& setPosition(unsigned pos) { 
        assert(cur >= 0 && cur <= size);
        cur = pos;
        return *this;
      }

      ByteStream subStream(unsigned start, unsigned end) const {
        return ByteStream(bytes + start, end - start);
      }

      void jump(int offset) { 
        cur += offset;
        assert(cur >= 0 && cur <= size);
      }
      
    private:
      const char* bytes;
      const unsigned size;
      unsigned cur;
    };
  }
}

#endif

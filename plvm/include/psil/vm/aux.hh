#ifndef PSIL_VM_AUX_HH
#define PSIL_VM_AUX_HH

#include <cstddef>

#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

namespace psil {
  namespace vm {
    namespace aux {
      template<class T>
      class SmartPtr {
      public:
        SmartPtr(T* p) : p(p) {}
        ~SmartPtr() { delete p; }
        T* get() const { return p; }
        T& getRef() const { return *p; }
        bool isNull() const { return p == NULL; }

      private:
        T* p;
      };

      struct Mmap {
        Mmap(const char* path, bool write_mode=false, int flags=MAP_SHARED) 
          : ptr(NULL), size(0) {
          int OPEN_MODE=O_RDONLY;
          int PROT = PROT_READ;
          if(write_mode) {
            OPEN_MODE=O_RDWR;
            PROT |= PROT_WRITE;
          }
          
          int f = open(path, OPEN_MODE);
          struct stat statbuf;
          fstat(f, &statbuf);
          ptr = mmap(0, statbuf.st_size, PROT, flags, f, 0);
          size=statbuf.st_size;
          close(f);
        }
        
        ~Mmap() { munmap(ptr, size); }
    
        operator bool () const { return ptr!=reinterpret_cast<void*>(-1); }

        void *ptr;        
        size_t size;
      };

      // XXX: int, shortのバイト数を仮定
      struct Endian {
        static int toBigInt(int n) {
#ifndef __BIG_ENDIAN__
          return
            ((n & 0xFF000000) >> 24) |
            ((n & 0x00FF0000) >>  8) |
            ((n & 0x0000FF00) <<  8) |
            ((n & 0x000000FF) << 24);
#else
          return n;
#endif
        }

        static short toBigShort(short n) {
#ifndef __BIG_ENDIAN__
          return
            ((n & 0xFF00) >> 8) |
            ((n & 0x00FF) << 8);
#else
          return n;
#endif
        }
      };
    }
  }
}

#endif

#ifndef PSIL_CORE_UTIL_MMAP_T_HH
#define PSIL_CORE_UTIL_MMAP_T_HH

#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

namespace psil {
namespace core {
namespace util {
  template<typename T>
  class mmap_t {
  public:
    mmap_t(const char* path, bool write_mode=false, int flags=MAP_SHARED){
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
    
    ~mmap_t() { munmap(ptr, size); }
    
    operator bool () const { return ptr!=reinterpret_cast<void*>(-1); }

    T* start() { return reinterpret_cast<char*>(ptr); }
    const T* end() { return reinterpret_cast<const T*>(reinterpret_cast<char*>(ptr)+size); }
    
  private:
    size_t size;
    void *ptr;
  };
}
}
}

#endif

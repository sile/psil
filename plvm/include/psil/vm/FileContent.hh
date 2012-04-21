#ifndef PSIL_VM_FILE_CONTENT_HH
#define PSIL_VM_FILE_CONTENT_HH

#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

namespace psil {
  namespace vm {
    class FileContent {
    public:
        FileContent(const char* path, bool write_mode=false, int flags=MAP_SHARED) 
          : ptr(NULL), len(0) {
          int PROT = PROT_READ;
          int f = open(path, O_RDONLY);
          struct stat statbuf;
          fstat(f, &statbuf);
          ptr = mmap(0, statbuf.st_size, PROT, flags, f, 0);
          len = statbuf.st_size;
          close(f);
        }
        
        ~FileContent() { munmap(ptr, len); }
    
        operator bool () const { return ptr!=reinterpret_cast<void*>(-1); }

    public:
      const char* bytes() const { return reinterpret_cast<const char*>(ptr); }
      unsigned size() const { return len; }
      
    private:
        void *ptr;        
        size_t len;      
    };
  }
}

#endif

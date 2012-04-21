#ifndef PSIL_VM_BYTECODE_OBJECT_HH
#define PSIL_VM_BYTECODE_OBJECT_HH

#include "aux.hh"
#include "ConstantTable.hh"
#include "CodeStream.hh"
#include <cstddef>
#include <cassert>
#include <cstring>

/*
  [format]
  - header
   - magic: "psil"
   - version: 1#4byte
   - constant-start: #4byte
   - constant-count: #4byte
   - code-start: #4byte
   - code-size: #4byte
  - body
   - constant*
   - code
 */
#define MAGIC_CODE "psil"

namespace psil {
  namespace vm {
    class BytecodeObject {
    public:
      struct Header {
        Header(const Header* h) 
          : version(aux::Endian::toBigInt(h->version)),
            constant_start(aux::Endian::toBigInt(h->constant_start)),
            constant_count(aux::Endian::toBigInt(h->constant_count)),
            code_start(aux::Endian::toBigInt(h->code_start)),
            code_size(aux::Endian::toBigInt(h->code_size))
        {
          strncpy(magic, h->magic, 4);
        }

        char magic[4];
        unsigned version;
        unsigned constant_start;
        unsigned constant_count;
        unsigned code_start;
        unsigned code_size;
      };
      
    protected:
      BytecodeObject(const char* bytes, unsigned size)
        : h(reinterpret_cast<const Header*>(bytes)), 
          const_table(NULL), code_stream(NULL) {
        assert(strcmp(MAGIC_CODE, h.magic) == 0);
        
        const_table = ConstantTable::parse(bytes + h.constant_start, h.constant_count);
        assert(const_table);

        code_stream = new CodeStream(bytes + h.code_start, h.code_size);
      }

    public:
      ~BytecodeObject() {
        delete const_table;
        delete code_stream;
      }

    private:
      const Header h;
      const ConstantTable* const_table;
      const CodeStream* code_stream;
    };

    class FileBytecodeObject : public BytecodeObject {
    protected:
      FileBytecodeObject(aux::Mmap* mm)
        : BytecodeObject(reinterpret_cast<const char*>(mm->ptr), mm->size), mm(mm) {}
      ~FileBytecodeObject() {
        delete mm;
      }
      
    public:
      static BytecodeObject* parse(const char* filepath) {
        aux::Mmap* mm = new aux::Mmap(filepath);

        if(! *mm) {
          return NULL;
        }
        return new FileBytecodeObject(mm);
      }

    private:
      aux::Mmap* mm;
    };
  }
}
#endif

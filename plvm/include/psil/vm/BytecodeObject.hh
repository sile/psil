#ifndef PSIL_VM_BYTECODE_OBJECT_HH
#define PSIL_VM_BYTECODE_OBJECT_HH

#include "ConstantTable.hh"
#include "ByteStream.hh"
#include <cassert>
#include <cstring>
#include <string>

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
#include <iostream>
namespace psil {
  namespace vm {
    class BytecodeObject {
    public:
      struct Header {
        static Header read(ByteStream& in) {
          char magic[4];
          in.readBytes(magic, 4);
          assert(strcmp(MAGIC_CODE, magic) == 0);
          
          Header h = {in.readUint4(),
                      in.readUint4(),
                      in.readUint4(),
                      in.readUint4(),
                      in.readUint4()};
          return h;
        }

        const unsigned version;
        const unsigned constant_start;
        const unsigned constant_count;
        const unsigned code_start;
        const unsigned code_size;
      };
      
    public:
      BytecodeObject(ByteStream& in) 
        : header(Header::read(in)), const_table(in, header.constant_count), 
          codeStream(in.subStream(header.code_start, header.code_start + header.code_size)) {}
      
      operator bool () const { return true; } // XXX:

      const ConstantTable& constantTable() const { return const_table; }

      std::string show() const;

    private:
      const Header header;
      const ConstantTable const_table;
      ByteStream codeStream;
    };
  }
}
#endif

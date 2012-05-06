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
          assert(strncmp(MAGIC_CODE, magic, strlen(MAGIC_CODE)) == 0);
          
          /*
          Header h = {in.readUint4(),
                      in.readUint4(),
                      in.readUint4(),
                      in.readUint4(),
                      in.readUint4()};
          */
          unsigned data[] = {in.readUint4(),
                             in.readUint4(),
                             in.readUint4(),
                             in.readUint4(),
                             in.readUint4()};
          Header h(data[0], data[1], data[2], data[3], data[4]);
          return h;
        }

        const unsigned version;
        const unsigned constant_start;
        const unsigned constant_count;
        const unsigned code_start;
        const unsigned code_size;

        // XXX:
        Header(unsigned version, unsigned constant_start, unsigned constant_count,
               unsigned code_start, unsigned code_size)
          : version(version), constant_start(constant_start), constant_count(constant_count),
            code_start(code_start), code_size(code_size) {}
      };
      
    public:
      BytecodeObject(ByteStream& in) 
        : header(Header::read(in)), const_table(in, header.constant_count),
          in(in.subStream(header.code_start, header.code_start + header.code_size)) {}
      
      operator bool () const { return true; } // XXX:

      const ConstantTable& constantTable() const { return const_table; }

      ByteStream& getCodeStream() { return in; }

      std::string show() const;
      
      // XXX: 実行時コード評価のための仕組みは後でもっとちゃんと考える
      static BytecodeObject& getRuntime() { return runtimeBcobj; }
      static void appendRuntimeCode(const char* bytes, unsigned size) {
        unsigned new_start_pos = runtimeBcobj.in.getSize(); 
        runtimeBcobj.in.append(bytes, size);
        runtimeBcobj.in.setPosition(new_start_pos);
      }

    private:
      BytecodeObject() : header(0,0,0,0,0), in(ByteStream::emptyStream()) {}

    private:
      const Header header;
      const ConstantTable const_table;
      ByteStream in;

      static BytecodeObject runtimeBcobj;
    };
  }
}
#endif

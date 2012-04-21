#ifndef PSIL_VM_BYTECODE_OBJECT_HH
#define PSIL_VM_BYTECODE_OBJECT_HH

namespace psil {
  namespace vm {
    class BytecodeObject {
    public:
      BytecodeObject(const char* bytes, unsigned size) {
      }
    };

    class FileBytecodeObject : public BytecodeObject {
    public:
      FileBytecodeObject(const char* filepath) : BytecodeObject(0, 0) {
      }

    private:
    };
  }
}
#endif

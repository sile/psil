#ifndef PSIL_CORE_BYTECODE_HH
#define PSIL_CORE_BYTECODE_HH

namespace psil {
  namespace core {
    /*
      [[format]]
      [header]
      version: 4byte
      
     */
    class bytecode {
    public:
      enum TYPE {
        TYPE_SYMBOL=0,
        TYPE_INT,
        TYPE_QUOTE,
        TYPE_NIL
      };

      struct header {
        int version;
        int symbol_count;
      };
    };
  }
}

#endif

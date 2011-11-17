#ifndef PSIL_CORE_TOKEN_TOKENIZER_HH
#define PSIL_CORE_TOKEN_TOKENIZER_HH

#include "token.hh"

namespace psil {
namespace core {
namespace token {
  class Tokenizer {
  public:
    Tokenizer() {
    }
    ~Tokenizer() {
    }
    
    bool tokenize(const char* source, Token* token) {
       return false;
    }
  };
}
}
}

#endif

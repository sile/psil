#ifndef PSIL_CORE_TOKEN_TOKENIZER_HPP
#define PSIL_CORE_TOKEN_TOKENIZER_HPP

#include "token.hpp"

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

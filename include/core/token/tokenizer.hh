#ifndef PSIL_CORE_TOKEN_TOKENIZER_HH
#define PSIL_CORE_TOKEN_TOKENIZER_HH

#include "token.hh"
#include "../util/char_stream.hh"

namespace psil {
namespace core {
namespace token {
  class Tokenizer {
  public:
    Tokenizer() {
    }
    ~Tokenizer() {
    }
    
    bool tokenize(util::CharStream& in, Token* token) {
       return false;
    }
  };
}
}
}

#endif

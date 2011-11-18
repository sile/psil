#ifndef PSIL_CORE_TOKEN_TOKENIZER_HH
#define PSIL_CORE_TOKEN_TOKENIZER_HH

#include "token.hh"
#include "../util/char_stream.hh"

namespace psil {
namespace core {
namespace token {
  using namespace util;

  class Tokenizer {
  public:
    Tokenizer(const char* start, const char* end) : in(start,end), root(NULL) {
    }

    ~Tokenizer() {
        delete root;
    }
    
    const Token* tokenize();

  private:
    CharStream in;
    Token* root;  
  };
}
}
}

#endif

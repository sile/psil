#include "core/token/tokenizer.hh"
#include <cstring>

namespace psil {
namespace core {
namespace token {
  namespace {
    bool is_integer_token(CharStream& in) {
      CharStream::Mark m = in.position();
      bool is_integer=true;
      while(isspace(in.peek())==false) {
        if(isdigit(in.read())==false) {
          is_integer=false;
          break;
        }
      }
      in.position(m);
      return is_integer;
    }
    
    bool is_real_token(CharStream& in) { return true; }
    Token* tokenize_integer(CharStream& in) { return NULL; }
    Token* tokenize_real(CharStream& in) { return NULL; }
    Token* tokenize_symbol(CharStream& in) { return NULL; }

    void tokenize_impl(CharStream& in, Token*& token) {
      if(isdigit(in.peek())) {
        // integer or real or symbol  
        if(is_integer_token(in)) {
          token = tokenize_integer(in);
        } else if(is_real_token(in)) {
          token = tokenize_real(in);
        } else {
          // assume as symbol
          token = tokenize_symbol(in);
        }
      }
    } 
  }

  const Token* Tokenizer::tokenize() {
    tokenize_impl(in, root);
    return root;
  }
}
}
}

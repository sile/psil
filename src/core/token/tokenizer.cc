#include "core/token/tokenizer.hh"
#include <cstring>
#include <cstdlib>
#include <string>

// XXX: for debug
#include <iostream>

namespace psil {
namespace core {
namespace token {
  namespace {
    bool is_delimiter(char c) {
      return isspace(c) || c=='(' || c=='\'' || c==')';
    }
    
    bool is_integer_token(CharStream& in) {
      CharStream::Mark m = in.position();
      while(is_delimiter(in.peek())==false)
        if(isdigit(in.read())==false) {
          in.position(m);
          return false;
        }
      in.position(m);
      return true;
    }
    
    void skip_space(CharStream& in) {
      while(in.is_eos()==false && isspace(in.peek()))
        in.read();
    }

    // XXX: accepts only 'ddd.ddd' format
    bool is_real_token(CharStream& in) { 
      CharStream::Mark m = in.position();
      int dot_count = 0;

      while(is_delimiter(in.peek())==false) {
        const char c = in.read();
        if(isdigit(c)==false) {
          if(c != '.')
            goto failed;
          dot_count++;
        }
      }
      if(dot_count != 1)
        goto failed;
      in.position(m);      
      return true; 
    failed:
      in.position(m);
      return false;
    }

    std::string read_until_next_delimiter(CharStream& in) {
      CharStream::Mark start = in.position();
      while(is_delimiter(in.peek())==false)
        in.read();
      return std::string(start, in.position());
    }
    
    Token* tokenize_integer(CharStream& in) {
      return new TokenInt(atoi(read_until_next_delimiter(in).c_str()));
    }

    Token* tokenize_symbol(CharStream& in) {
      return new TokenSym(read_until_next_delimiter(in));
    }

    Token* tokenize_real(CharStream& in) {
      return new TokenReal(atof(read_until_next_delimiter(in).c_str()));
    }

    Token* tokenize_string(CharStream& in) {
      in.read(); // eat start '"'
      CharStream::Mark start = in.position();
      while(in.peek() != '"') {
        if(in.is_eos())
          throw "EOS reached!";
        in.read();
      }
      CharStream::Mark end = in.position();
      in.read(); // eat end '"'
      return new TokenStr(std::string(start,end));
    }

    Token* tokenize_impl(CharStream& in) {
      skip_space(in);
      
      if(isdigit(in.peek())) {
        // integer or real or symbol  
        if(is_integer_token(in))
          return tokenize_integer(in);
        if(is_real_token(in))
          return tokenize_real(in);
      } else if (in.peek()=='"') {
        return tokenize_string(in);
      }
      
      // assume as symbol
      return tokenize_symbol(in);
    } 
  }

  const Token* Tokenizer::tokenize() {
    root = tokenize_impl(in);
    return root;
  }
}
}
}

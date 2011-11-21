#include "core/read/reader.hh"
#include "core/util/char_stream.hh"
#include <cstring>
#include <cstdlib>
#include <string>

// XXX: for debug
#include <iostream>

namespace psil {
namespace core {
  namespace object {
    // XXX:
    const Cons* Cons::NIL = new Cons;
  }

namespace read {
  using namespace util;
  using namespace object;
  

  namespace {
    bool is_delimiter(char c) {
      return isspace(c) || c=='(' || c=='\'' || c==')';
    }
    
    bool is_integer_obj(CharStream& in) {
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
    bool is_real_obj(CharStream& in) { 
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
    
    Integer* read_integer(CharStream& in) {
      return new Integer(atoi(read_until_next_delimiter(in).c_str()));
    }

    Symbol* read_symbol(CharStream& in) {
      return new Symbol(read_until_next_delimiter(in));
    }

    Real* read_real(CharStream& in) {
      return new Real(atof(read_until_next_delimiter(in).c_str()));
    }

    String* read_string(CharStream& in) {
      in.read(); // eat start '"'
      CharStream::Mark start = in.position();
      while(in.peek() != '"') {
        if(in.is_eos())
          throw "EOS reached!";
        in.read();
      }
      CharStream::Mark end = in.position();
      in.read(); // eat end '"'
      return new String(std::string(start,end));
    }

    const Object* read_impl(CharStream&);
    const Cons* read_list(CharStream& in) {
      if(in.is_eos()) 
          throw "EOS reached!";
      
      if(skip_space(in),in.peek()==')') {
        in.read(); // eat ')'
        return Cons::NIL;
      }
      
      return new Cons(read_impl(in), read_list(in));
    }

    const Object* read_impl(CharStream& in) {
      skip_space(in);
      
      if(isdigit(in.peek())) {
        // integer or real or symbol  
        if(is_integer_obj(in))
          return read_integer(in);
        if(is_real_obj(in))
          return read_real(in);
      } else if (in.peek()=='"') {
        return read_string(in);
      } else if (in.peek()=='(') {
        in.read(); // eat '('
        return read_list(in);
      } else if (in.peek()=='\'') {
        in.read(); // eat '\''
        return new Cons(new Symbol("QUOTE"), read_impl(in));
      }
      
      // assume as symbol
      return read_symbol(in);
    } 
  }

  const Object* Reader::read(const char* start, const char* end)  {
    CharStream cs(start, end);
    return read_impl(cs);
  }
}
}
}

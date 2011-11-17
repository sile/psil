#ifndef PSIL_CORE_TOKEN_TOKEN_HH
#define PSIL_CORE_TOKEN_TOKEN_HH

#include <string>
#include <vector>

namespace psil {
namespace core {
namespace token {
  /*
   * 数:
   *  - 整数
   *  - 実数
   *  - etc
   * 文字列:
   * リスト:
   * 式:
   * 変数:
   */
  class Token {
  public:
    enum TYPE {INT, REAL, SYM, STR, LIST, EXP};
    virtual TYPE type() = 0;
  };

  class TokenInt : public Token {
  public:
    TokenInt(int val) : val(val) {}
    virtual TYPE type() { return INT; }
    int value() { return val; }

  private:
    int val; 
  };

  class TokenReal : public Token {
  public:
    TokenReal(double val) : val(val) {}
    virtual TYPE type() { return REAL; }
    double value() { return val; }
  
  private:
    double val;
  };

  class TokenSym : public Token {
  public:
    TokenSym(const std::string& val) : val(val) {}
    virtual TYPE type() { return SYM; }
    const std::string& value() { return val; }
 
  private:
    std::string val;
  };

  class TokenStr : public Token {
  public:
    TokenStr(const std::string& val) : val(val) {}
    virtual TYPE type() { return STR; }
    const std::string& value() { return val; }

  private:
    std::string val;
  };

  typedef std::vector<Token*> List;
  class TokenList : public Token {
  public:
    TokenList(const List& val) : val(val) {}
    virtual TYPE type() { return LIST; }
    const List& value() { return val; }

  private:
    List val;
  };

  class TokenExp : public Token {
  public:
    TokenExp(const List& val) : val(val) {}
    virtual TYPE type() { return EXP; }
    const List& value() { return val; }

  private:
    List val;
  };
}
}
}

#endif
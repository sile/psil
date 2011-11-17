#ifndef PSIL_CORE_TOKEN_TOKEN_HH
#define PSIL_CORE_TOKEN_TOKEN_HH

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
    enum TYPE {INT, REAL, VAR, STR, LIST, EXP};
    virtual TYPE type() = 0;
  };

  class TokenInt : public Token {
  public:
    virtual TYPE type() { return INT; }
  };

  class TokenReal : public Token {
  public:
    virtual TYPE type() { return REAL; }
  };

  class TokenVar : public Token {
  public:
    virtual TYPE type() { return VAR; }
  };

  class TokenStr : public Token {
  public:
    virtual TYPE type() { return STR; }
  };

  class TokenList : public Token {
  public:
    virtual TYPE type() { return LIST; }
  };

  class TokenExp : public Token {
  public:
    virtual TYPE type() { return EXP; }
  };
}
}
}

#endif

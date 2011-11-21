#ifndef PSIL_CORE_OBJECT_OBJECT_HH
#define PSIL_CORE_OBJECT_OBJECT_HH

// for dev
#include "../util/str.hh"

#include <string>

namespace psil {
namespace core {
namespace object {
  class Object {
  public:
    enum TYPE {INTEGER, REAL, SYMBOL, STRING, CONS};
    virtual ~Object() {}
    virtual TYPE type() = 0;

    // for dev
    virtual std::string toString() const { return ""; }
  };

  class Integer : public Object {
  public:
    Integer(int val) : val(val) {}
    virtual TYPE type() { return INTEGER; }
    int value() { return val; }

    virtual std::string toString() const {
      return "int#"+util::Str::toString(val);
    }
    
  private:
    int val; 
  };

  class Real : public Object {
  public:
    Real(double val) : val(val) {}
    virtual TYPE type() { return REAL; }
    double value() { return val; }

    virtual std::string toString() const {
      return "real#"+util::Str::toString(val);
    }
  
  private:
    double val;
  };

  class Symbol : public Object {
  public:
    Symbol(const std::string& val) : val(val) {}
    virtual TYPE type() { return SYMBOL; }
    const std::string& value() { return val; }
    virtual std::string toString() const {
      return "sym#"+util::Str::toString(val);
    } 
    
  private:
    std::string val;
  };

  class String : public Object {
  public:
    String(const std::string& val) : val(val) {}
    virtual TYPE type() { return STRING; }
    const std::string& value() { return val; }

    virtual std::string toString() const {
      return "str#\""+val+"\"";
    } 

  private:
    std::string val;
  };

  class Cons : public Object {
  public:
    static const Cons* NIL;

  public:
    Cons(const Object* car, const Object* cdr) : car(car), cdr(cdr) {}
    ~Cons() {
      if(car != NIL)
        delete car;
      if(cdr != NIL)
        delete cdr;
    }
    virtual TYPE type() { return CONS; }
    const Object* car_value() { return car; }
    const Object* cdr_value() { return cdr; }

    virtual std::string toString() const {
      if(this == NIL)
        return "NIL";
      std::string buf = "cons#(";
      buf += car->toString();
      buf += " . "+cdr->toString();
      return buf+")";
    } 

    //  private:
    Cons() : car(this), cdr(this) {}

  private:
    const Object* car;
    const Object* cdr;
  };
  
}
}
}

#endif

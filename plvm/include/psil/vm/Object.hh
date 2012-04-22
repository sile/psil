#ifndef PSIL_VM_OBJECT_HH
#define PSIL_VM_OBJECT_HH

#include <string>
#include "type.hh"

namespace psil {
  namespace vm {
    namespace type {
      enum OBJ_TYPE {
        TYPE_UNDEF,
        TYPE_INT,
        TYPE_STRING,
        TYPE_CHAR,
        TYPE_SYMBOL
      };

      // Object
      class Object {
      public:
        virtual OBJ_TYPE getType() const = 0;
        virtual std::string show() const = 0;
        virtual ~Object() {}
      };

      // Undef
      class Undef : public Object {
      public:
        static Undef* make() { return &undef; }
        OBJ_TYPE getType() const { return TYPE_UNDEF; }
        std::string show() const { return std::string("<UNDEF>"); }

      private:
        static Undef undef;
      };
      
      // Int
      class Int : public Object {
      private:
        Int(uint4 n) : value(n) {}

      public:
        static Int* make(uint4 n) { return new Int(n); }
        OBJ_TYPE getType() const { return TYPE_INT; }
        std::string show() const;
        
        uint4 getValue() const { return value; }
        
      private:
        uint4 value;
      };
      
      // String
      class String : public Object {
      private:
        String(const std::string& s) : value(s) {}

      public:
        static String* make(const std::string& s) { return new String(s); }
        OBJ_TYPE getType() const { return TYPE_STRING; }
        std::string show() const { return std::string("<STRING ")+value+">"; }
        
        const std::string& getValue() const { return value; }
        
      private:
        std::string value;
      };

      // Character
      // # ユニコード。ただしStringとの相互連携が面倒なので、現状はascii範囲のみを想定
      class Char : public Object {
      private:
        Char(uint4 code) : code(code) {}
        
      public:
        static Char* make(uint4 code) { return new Char(code); }
        OBJ_TYPE getType() const { return TYPE_CHAR; }
        std::string show() const;
        
        uint4 getCode() const { return code; }

      private:
        uint4 code;
      };

      // Symbol
      class Symbol : public Object {
      private:
        Symbol(const std::string& name) : name(name), value(Undef::make()) {} 

      public:
        static Symbol* make(const std::string& name);
        OBJ_TYPE getType() const { return TYPE_SYMBOL; }
        std::string show() const { return std::string("<SYMBOL ") + name + ">"; }

        const std::string& getName() const { return name; }
        Object* getValue() const { return value; }

      private:
        const std::string name;
        Object* value;
      };
    }
  }
}

#endif

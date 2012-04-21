#ifndef PSIL_VM_OBJECT_HH
#define PSIL_VM_OBJECT_HH

#include <string>

namespace psil {
  namespace vm {
    namespace type {
      enum OBJ_TYPE {
        TYPE_UNDEF,
        TYPE_SYMBOL
      };

      class Object {
      public:
        virtual OBJ_TYPE getType() const = 0;
        virtual std::string show() const = 0;
      };

      class Undef : public Object {
      public:
        static Undef* make() { return &undef; }
        OBJ_TYPE getType() const { return TYPE_UNDEF; }
        std::string show() const { return std::string("<undef>"); }

      private:
        static Undef undef;
      };
      
      class Symbol : public Object {
      private:
        Symbol(const std::string& name) : name(name), value(Undef::make()) {} 

      public:
        static Symbol* make(const std::string& name);
        OBJ_TYPE getType() const { return TYPE_SYMBOL; }
        std::string show() const { return std::string("<SYMBOL ") + name + ">"; }
        
      public:
        const std::string name;
        Object const * value;
      };
    }
  }
}

#endif

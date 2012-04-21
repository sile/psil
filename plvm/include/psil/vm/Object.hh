#ifndef PSIL_VM_OBJECT_HH
#define PSIL_VM_OBJECT_HH

#include <string>

namespace psil {
  namespace vm {
    namespace type {
      enum OBJ_TYPE {
        TYPE_SYMBOL
      };

      class Object {
      public:
        virtual OBJ_TYPE getType() const = 0;
        virtual std::string show() const = 0;
      };
      
      class Symbol : public Object {
      public:
        Symbol(const std::string& name) : name(name), value(NULL) {
        }
        
        static Symbol* intern(const std::string& name) {
          // TODO: シンボルテーブル検索
          return new Symbol(name);
        }
        
        OBJ_TYPE getType() const { return TYPE_SYMBOL; }

        std::string show() const { 
          return std::string("<SYMBOL ") + name + ">";
        }

      private:
        std::string name;
        Object* value;
      };
    }
  }
}

#endif

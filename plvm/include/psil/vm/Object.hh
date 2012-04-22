#ifndef PSIL_VM_OBJECT_HH
#define PSIL_VM_OBJECT_HH

#include <string>
#include "type.hh"

namespace psil {
  namespace vm {
    class Environment;
    class BytecodeObject;
    typedef BytecodeObject Context;

    namespace type {
      enum OBJ_TYPE {
        TYPE_UNDEF,
        TYPE_INT,
        TYPE_STRING,
        TYPE_CHAR,
        TYPE_SYMBOL,
        TYPE_NIL,
        TYPE_BOOL,
        TYPE_CONS,
        TYPE_ARRAY,
        TYPE_LAMBDA,
        TYPE_NATIVE_LAMBDA,
        TYPE_REF
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
        Int(int4 n) : value(n) {}

      public:
        static Int* make(int4 n) { return new Int(n); }
        OBJ_TYPE getType() const { return TYPE_INT; }
        std::string show() const;
        
        int4 getValue() const { return value; }
        
      private:
        int4 value;
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
        Object* setValue(Object* o) { return value=o; }

      private:
        const std::string name;
        Object* value;
      };

      // Nil
      class Nil : public Object {
      private:
        Nil() {}

      public:
        static Nil* make() { return &nil; }
        OBJ_TYPE getType() const { return TYPE_NIL; }
        std::string show() const { return "<NIL>";}
        
      private:
        static Nil nil;
      };

      // Boolean
      class Boolean : public Object {
      private:
        Boolean() {}
        
      public:
        static Boolean* make(bool b) { return b ? &_true : &_false; }
        OBJ_TYPE getType() const { return TYPE_BOOL; }
        std::string show() const { return this == &_true ? "<TRUE>" : "<FALSE>"; }
        
        static bool isFalse(const Object* x) { return x == &_false; }

      private:
        static Boolean _true;
        static Boolean _false;
      };

      // Cons
      class Cons : public Object {
      private:
        Cons(Object* car, Object* cdr) : car(car), cdr(cdr) {}

      public:
        static Cons* make(Object* car, Object* cdr) { return new Cons(car, cdr); }
        OBJ_TYPE getType() const { return TYPE_CONS; }
        std::string show() const;
        
        Object* getCar() const { return car; }
        Object* getCdr() const { return cdr; }
        
      private:
        Object* car;
        Object* cdr;
      };

      // Array
      class Array : public Object {
      private:
        Array(uint4 size) : ary(NULL), size(size) {
          ary = new Object*[size];
          for(uint4 i=0; i < size; i++) {
            ary[i] = Undef::make();
          }
        }
        ~Array() {
          delete [] ary;
        }

      public:
        static Array* make(uint4 size) { return new Array(size); }
        OBJ_TYPE getType() const { return TYPE_ARRAY; }
        std::string show() const;
        
        Object* get(uint4 index) const { return ary[index]; }
        void set(uint4 index, Object* value) { ary[index] = value; }
        uint4 getSize() const { return size; }
        
      private:
        Object** ary;
        uint4 size;
      };

      // Lambda
      class Lambda : public Object {
      public:
        struct Closed {
          Closed(Object** vals, uint1 count) : vals(vals), val_count(count) {}
          Object** vals;
          uint1 val_count;
        };
        
      private:
        Lambda(Closed closed, uint1 arity, uint1 local_var_count, 
               unsigned body_addr, Context* context) 
          : closed(closed), arity(arity), local_var_count(local_var_count), 
            body_addr(body_addr), context(context) {}
        
        ~Lambda() {
          delete [] closed.vals;
        }

      public:
        static Lambda* make(Object** closed_vals, uint1 closed_val_count,
                            uint1 arity, uint1 local_var_count, 
                            unsigned body_addr, Context* context) {
          return new Lambda(Closed(closed_vals, closed_val_count),
                            arity, local_var_count, body_addr, context);
        }
        OBJ_TYPE getType() const { return TYPE_LAMBDA; }
        std::string show() const;
        
        Object* getClosedValue(uint1 i) const { return closed.vals[i]; }
        uint1 getClosedValueCount() const { return closed.val_count; }
        uint1 getArity() const { return arity; }
        uint1 getLocalVarCount() const { return local_var_count; }
        unsigned getBodyAddress() const { return body_addr; }
        Context* getContext() { return context; }
        
      private:
        Closed closed;
        uint1 arity;  // 不要かも
        uint1 local_var_count; // TODO: コンパイラが賢ければ不要 (body内でのstackの使い方を追跡) 
        unsigned body_addr;
        Context* context;
      };

      // NativeLambda
      typedef void (*NATIVE_FUN_T) (Environment&);
      class NativeLambda : public Object {
      private:
        NativeLambda(NATIVE_FUN_T body) : body(body) {}
        
      public:
        static NativeLambda* make(NATIVE_FUN_T body) { return new NativeLambda(body); }
        OBJ_TYPE getType() const { return TYPE_NATIVE_LAMBDA; }
        std::string show() const { return std::string("<NATIVE_LAMBDA>"); }
        
        NATIVE_FUN_T getBody() const { return body; };

      private:
        NATIVE_FUN_T body;
      };

      // 
      class Reference : public Object {
      private:
        Reference(Object* x) : value(x) {}
        
      public:
        static Reference* make(Object* x) { return new Reference(x); }
        OBJ_TYPE getType() const { return TYPE_REF; }
        std::string show() const { return std::string("<REF ") + value->show() + ">"; }
        
        Object* getValue() const { return value; }
        Object* setValue(Object* x) { return value=x; }

      private:
        Object* value;
      };
      
      //
      namespace {
        template<class T> OBJ_TYPE getType() { return TYPE_UNDEF; }
        template<> OBJ_TYPE getType<Int>() { return TYPE_INT; }
        template<> OBJ_TYPE getType<String>() { return TYPE_STRING; }
        template<> OBJ_TYPE getType<Char>() { return TYPE_CHAR; }
        template<> OBJ_TYPE getType<Symbol>() { return TYPE_SYMBOL; }
        template<> OBJ_TYPE getType<Nil>() { return TYPE_NIL; }
        template<> OBJ_TYPE getType<Cons>() { return TYPE_CONS; }
        template<> OBJ_TYPE getType<Boolean>() { return TYPE_BOOL; }
        template<> OBJ_TYPE getType<Array>() { return TYPE_ARRAY; }
        template<> OBJ_TYPE getType<Lambda>() { return TYPE_LAMBDA; }
        template<> OBJ_TYPE getType<NativeLambda>() { return TYPE_NATIVE_LAMBDA; }
        template<> OBJ_TYPE getType<Reference>() { return TYPE_REF; }
      }
      
      //
      template<class T>
      T* to(Object* o) {
        assert(o->getType() == getType<T>());
        return reinterpret_cast<T*>(o);
      }
    }
  }
}

#endif

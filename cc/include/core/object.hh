#ifndef PSIL_CORE_OBJECT_HH
#define PSIL_CORE_OBJECT_HH

#include "util.hh"
#include <string>
#include <istream>

namespace psil {
  namespace core {
    namespace obj {
      int read_int(std::istream& in) {
        int n;
        in.read(reinterpret_cast<char*>(&n), sizeof(int));
        return n;
      }
      
      std::string& read_str(std::istream& in, int count, std::string& buf) {
        buf.resize(count);
        in.read(const_cast<char*>(buf.c_str()), count);
        return buf;
      }

      enum OBJ_TYPE {
        O_OBJECT=0,
        O_CONS,
        O_LIST,
        O_STRING,
        O_REFER,
        O_INTEGER,
        O_SYMBOL
      };

      class object {
      public:
        virtual OBJ_TYPE type() const {
          return m_type;
        }
          
        virtual std::string& show(std::string& buf) {
          buf = "<<undef>>";
          return buf;
        }

        static object* read(std::istream& in) {
          ERR("Unexpected type: object");
        }

      protected:
        object(OBJ_TYPE type) : m_type(type) {}
        OBJ_TYPE m_type;
      };

      class symbol : public object {
      public:
        symbol(int code) : object(obj::O_SYMBOL), code(code) {
        }

        std::string& show(std::string& buf) {
          buf = "#<SYMBOL ";
          buf += util::Str::toString(code);
          buf += ">";
          return buf;
        }

        int value() const { return code; }

        static object* read(std::istream& in) {
          return new symbol(read_int(in));
        }
      private:
        int code;
      };

      class refer : public object {
      public:
        refer(object* x) : object(obj::O_REFER), x(x) {
        }
        
        object* get() const { return x; };

      private:
        object* x;
      };

      symbol NIL(0);
      
      class cons : public object {
      public:
        cons(object* car, object* cdr) 
          : object(obj::O_CONS),
            car(car), cdr(cdr) {
        }
             
        object* get_car() const { return car; }
        object* get_cdr() const { return cdr; }
        void set_cdr(object* _cdr) { cdr=_cdr; }
        void set_car(object* _car) { car=_car; }
      private:
        object* car;
        object* cdr;
      };
      
      class list : public object {
      public:
        list() : object(obj::O_LIST), head(&NIL) {
        }
        
        void push(object* x) {
          head = new cons(x, head);
        }

        int length() const {
          const object* cur=head;;
          int len=0;
          while(cur != &NIL) {
            cur = cdr(cur);
            len++;
          }
          return len;
        }

        void reverse() {
          object* cur = head;
          object* next = &NIL;
          while(cur != &NIL) {
            object* tmp = cdr(cur);
            reinterpret_cast<cons*>(cur)->set_cdr(next);
            next = cur;
            
            cur = tmp;
          }
          head = next;
        }
        
        object* value() const { return head; }
        
        static object* cdr(const object* x) {
          if(x==&NIL)
            return &NIL;
          return reinterpret_cast<const cons*>(x)->get_cdr();
        }
        static object* car(const object* x) {
          if(x==&NIL)
            return &NIL;
          return reinterpret_cast<const cons*>(x)->get_car();
        }

        bool operator==(const list& x) const {
          if(length() != x.length())
            return false;

          const object* a = head;
          const object* b = x.head;
          while(a != &NIL) {
            if(!(car(a) == car(b)))
              return false;
            a = cdr(a);
            b = cdr(b);
          }
          
          return true;
        }

      protected:
        list(OBJ_TYPE type) : object(type), head(&NIL) {
        }
      protected:
        object* head;
      };
      
      class integer : public object {
      public:
        integer(int n) : object(obj::O_INTEGER), n(n) {
        }
        
        int value() const { 
          return n; 
        }
        
      private:
        int n;
      };

      class string : public list {
      public:
        string() : list(obj::O_STRING) {
        }
        string(const char* src) : list(obj::O_STRING) {
          for(const char* c=src; *c != '\0'; c++) {
            push(new integer((int)*c&0xFF));
          }
          reverse();
        }
        
        std::string& show(std::string& buf) {
          buf = "\"";

          const object* cur=head;;
          while(cur != &NIL) {
            buf += (char)((integer*)car(cur))->value();
            cur = cdr(cur);
          }
          
          buf += "\"";
          return buf;
        }

        static object* read(std::istream& in) {
          int len = read_int(in);
          std::string buf;
          return new string(read_str(in, len, buf).c_str());
        }
      };

      object* read_object(std::istream& in) {
        int type = read_int(in);
        switch(type) {
        case O_CONS: return cons::read(in);
        case O_LIST: return list::read(in);
        case O_STRING: return string::read(in);
        case O_REFER: return refer::read(in);
        case O_INTEGER: return integer::read(in);
        case O_OBJECT: return object::read(in);
        case O_SYMBOL: return symbol::read(in);
        default:
          ERR(std::string("Unexpected type: ") + util::to_string(type));
        }
      }
    }
  }
}

#endif

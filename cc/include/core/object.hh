#ifndef PSIL_CORE_OBJECT_HH
#define PSIL_CORE_OBJECT_HH

#include "util.hh"
#include <string>
#include <istream>
#include <cassert>

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
      
      class object;
      object* read_object(std::istream& in);
      
      enum OBJ_TYPE {
        O_OBJECT=0,
        O_CONS,
        O_LIST,
        O_STRING,
        O_REFER,
        O_INTEGER,
        O_SYMBOL,
        O_QUOTE,
        O_FUNCTION,
        O_SPECIAL,
        O_MACRO_FUNCTION,
        O_NATIVE_FUNCTION
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
        
        std::string show() {
          std::string b;
          return show(b);
        }

        static object* read(std::istream& in) {
          ERR("Unexpected type: object");
        }

      protected:
        object(OBJ_TYPE type) : m_type(type) {}
        OBJ_TYPE m_type;
      };

      class special : public object {
      public:
        enum TYPE {
          LAMBDA = 0,
          PROGN = 1,
          IF = 2,
          LAMBDA_MACRO = 3
        };

        special(int code) : object(obj::O_SPECIAL), code(code) {}
        
        int value() const { 
          return code;
        }
        
        std::string& show(std::string& buf) {
          buf = "#<SPECIAL ";
          switch(code) {
          case LAMBDA:
            buf += "LAMBDA";
            break;
          case PROGN:
            buf += "PROGN";
            break;
          case IF:
            buf += "IF";
            break;
          case LAMBDA_MACRO:
            buf += "LAMBDA_MACRO";
            break;
          default:
            buf += "<<undef>>";
          }
          buf += ">";
          return buf;
        }
                  
        static object* read(std::istream& in) {
          return new special(read_int(in));
        }        

      private:
        int code;
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

      class quote : public object {
      public:
        quote(object* x) : object(obj::O_QUOTE), x(x) {
        }
        
        std::string& show(std::string& buf) {
          std::string b;
          buf = "#<QUOTE ";
          buf += x->show(b);
          buf += ">";
          return buf;
        }

        static object* read(std::istream& in) {
          return new quote(read_object(in));
        }

        object* value() const { return x; }

      private:
        object* x;
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
      
      bool is_nil(object* o) { 
        if(o==&NIL)
          return true;
        
        // TODO: eq
        return o->type()==obj::O_SYMBOL && ((symbol*)o)->value() == NIL.value();
      }
      
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

        list(object* o) : object(obj::O_LIST) {
          switch(o->type()) {
          case obj::O_LIST:
            head = ((list*)o)->head;
            break;
          case obj::O_CONS:
            head = o;
            break;
          case obj::O_SYMBOL:
            if(is_nil(o)) {
              head = o;
              break;
            }
          default:
            ERR(o->type() + " can't be converted to list");
          }
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
          if(x->type()==obj::O_LIST)
            x = ((list*)x)->value();
          return reinterpret_cast<const cons*>(x)->get_cdr();
        }
        static list* cdr_list(const object* x) {
          return new list(cdr(x));
        }

        static object* car(const object* x) {
          if(x==&NIL)
            return &NIL;
          if(x->type()==obj::O_LIST)
            x = ((list*)x)->value();
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

        std::string& show(std::string& buf) {
          std::string b;
          buf = "#<LIST";

          object* cur=head;
          while(cur != &NIL) {
            buf += " ";
            buf += car(cur)->show(b);
            cur = cdr(cur);
          }
          
          buf += ">";
          return buf;
        }
                  
        static object* read(std::istream& in) {
          list* lst = new list();
          int len = read_int(in);
          for(int i=0; i < len; i++) {
            lst->push(read_object(in));
          }
          lst->reverse();
          return lst;
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
        
        std::string& show(std::string& buf) {
          buf = util::to_string(n);
          return buf;
        }
                  
        static object* read(std::istream& in) {
          return new integer(read_int(in));
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

      class function : public object {
      public:
        function(list* params, list* body) 
          : object(obj::O_FUNCTION), params(params), body(body) {

          LIST_EACH(p, params, {
              if(p->type() != obj::O_SYMBOL)
                ERR(p->show() + " is not a symbol");
          });

          // add implicit progn
          body->push(new special(special::PROGN));
        }

        std::string& show(std::string& buf) {
          std::string b;
          buf = "#<FUNCTION ";
          buf += params->show(b);
          buf += " ";
          buf += body->show(b);
          buf += ">";
          return buf;
        }

        obj::list* get_params() const { return params; }
        obj::list* get_body() const { return body; }
        
      protected:
        obj::list* params;
        obj::list* body;
      };

      class macro_function : public function {
      public:
        macro_function(list* params, list* body) 
          : function(params, body) {
          m_type = obj::O_MACRO_FUNCTION;
        }

        std::string& show(std::string& buf) {
          std::string b;
          buf = "#<MACRO_FUNCTION ";
          buf += params->show(b);
          buf += " ";
          buf += body->show(b);
          buf += ">";
          return buf;
        }
      };

      class native_function : public object {
      public:
        native_function(int fn_index) 
          : object(obj::O_NATIVE_FUNCTION), fn_index(fn_index) {}
        
        obj::object* apply(NATIVE_FN* table, obj::list* args, environment* env) {
          return table[fn_index](args, env);
        }

        std::string& show(std::string& buf) {
          buf = "#<NATIVE_FUNCTION ";
          buf += util::to_string(fn_index);
          buf += ">";
          return buf;
        }
        
        static object* read(std::istream& in) {
          return new native_function(read_int(in));
        }               

      private:
        int fn_index;
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
        case O_QUOTE: return quote::read(in);
        case O_SPECIAL: return special::read(in);
        case O_NATIVE_FUNCTION: return native_function::read(in);
        default:
          ERR(std::string("Unexpected type: ") + util::to_string(type));
        }
      }

      bool is_integer(object* o) {
        return o->type() == obj::O_INTEGER;
      }
      
      integer* to_integer(object* o) {
        assert(is_integer(o));
        return (obj::integer*)o;
      }
    }
  }
}

#endif

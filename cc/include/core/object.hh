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
      symbol TRUE(1);
      
      bool is_nil(const object* o) { 
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

        std::string& show(std::string& buf);      

        bool is_proper_list() const {
          const cons* cur = this;
          for(;; cur = (const cons*)cur->cdr) {
            if(is_nil(cur->cdr))
              return true;
            if(cur->cdr->type() != obj::O_CONS)
              return false;
          }
        }

      private:
        object* car;
        object* cdr;
      };
      
      union list {
        cons u_cons;
        symbol u_nil;
        object u_obj;
        
        OBJ_TYPE type() const { return u_obj.type(); }
        bool is_null() const { return is_nil(&u_obj); }

        std::string& show(std::string& buf);
        static object* read(std::istream& in);

        int length() const;

        object* value() { return &u_obj; }
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

      struct lists {
        static list* reverse(list* cur) {
          object* next = &NIL;
          while(cur->is_null()==false) {
            object* tmp = cdr(cur);
            cur->u_cons.set_cdr(next);
            next = cur->value();
            // XXX:
            cur = (list*)tmp;
          }
          return (list*)next;
        }
        
        static object* car(list* x) {
          if(x->is_null())
            return &NIL;
          return x->u_cons.get_car();
        }
        
        static object* cdr(list* x) {
          if(x->is_null())
            return &NIL;
          return x->u_cons.get_cdr();          
        }

        static list* cdr_list(list* x) {
          // TODO: validation
          return to_list(cdr(x));
        }

        static list* to_list(object* x) {
          return reinterpret_cast<list*>(x);
        }

        static bool eql(const list* x, const list* y) {
          if(x->length() != y->length())
            return false;
          
          X_LIST_EACH2(a, b, x, y, {
              if(!(a==b))
                return false;
            });
          
          return true;
        }
      };

      int list::length() const {
        int len = 0;
        X_LIST_EACH(x, this, {
            assert(x); // XXX:
            len++;
          });
        return len-1;
      }
      object* list::read(std::istream& in) {
        list* head = lists::to_list(&NIL);
        int len = read_int(in);
        for(int i=0; i < len; i++) {
          head = lists::to_list(new cons(read_object(in), &head->u_obj));
        }
        return &lists::reverse(head)->u_obj;
      }   

      std::string& list::show(std::string& buf) {
          std::string b;
          buf = "#<LIST";
          
          X_LIST_EACH(x, this, {
              buf += " ";
              buf += x->show(b);
            });
          
          buf += ">";
          return buf;
      }

      std::string& cons::show(std::string& buf) {
        if(is_proper_list()) 
          return lists::to_list(this)->show(buf);
        
        std::string b;
        buf = "#<CONS";
        
        object* cur = this;
        for(; cur->type()==obj::O_CONS; cur = ((cons*)cur)->cdr) {
          buf += " ";
          buf += ((cons*)cur)->car->show(b);
        }
        buf += " . ";
        buf += cur->show(b);

        buf += ">";
        return buf;
      }

      class string : public object {
      public:
        string() : object(obj::O_STRING), head(lists::to_list(&NIL)) {
        }
        
        string(const char* src) : object(obj::O_STRING), head(lists::to_list(&NIL)) {
          for(const char* c=src; *c != '\0'; c++) {
            head = lists::to_list(new cons(new integer((int)*c&0xFF), &head->u_obj));
          }
          head = lists::reverse(head);
        }
        
        std::string& show(std::string& buf) {
          buf = "\"";

          X_LIST_EACH(x, head, {
              buf += ((integer*)x)->value();
            });
          
          buf += "\"";
          return buf;
        }

        static object* read(std::istream& in) {
          int len = read_int(in);
          std::string buf;
          return new string(read_str(in, len, buf).c_str());
        }

        bool operator==(const string& str) const {
          return lists::eql(head, str.head);
        }

      private:
        list* head;
      };

      class function : public object {
      public:
        function(list* params, list* body) 
          : object(obj::O_FUNCTION), params(params), body(body) {

          X_LIST_EACH(p, params, {
              if(p->type() != obj::O_SYMBOL)
                ERR(p->show() + " is not a symbol");
          });

          // add implicit progn
          body = lists::to_list(new cons(new special(special::PROGN), &body->u_obj));
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

      object* o_nil() { return &obj::NIL; }
      object* o_t() { return &obj::TRUE; }
      

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

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
        O_QUOTE,  // => special-form
        O_FUNCTION,
        O_SPECIAL,
        O_MACRO_FUNCTION,
        O_NATIVE_FUNCTION,
        O_STREAM,
        O_SYMBOL_MACRO
      };

      class object {
      public:
        virtual OBJ_TYPE type() const {
          return m_type;
        }
          
        virtual std::string& show(std::string& buf) const {
          buf = "<<undef>>";
          return buf;
        }
        
        std::string show() const {
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
          LAMBDA_MACRO = 3,
          QUOTE = 4,
          SYMBOL_MACRO = 5
        };

        special(int code) : object(obj::O_SPECIAL), code(code) {}
        
        int value() const { 
          return code;
        }
        
        std::string& show(std::string& buf) const {
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
          case QUOTE:
            buf += "QUOTE";
            break;
          case SYMBOL_MACRO:
            buf += "SYMBOL-MACRO";
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

      // TODO: internしないシンボルにも対応
      class string;
      class symbol : public object {
        typedef symbol* (*TableLookupFn)(symbol*);
        typedef symbol* (*TableLookupByNameFn)(string*);
        typedef symbol* (*TableLookupByCodeFn)(int);
        typedef symbol* (*TableLookupByNameFn2)(const char*);
        typedef const string* (*SymbolNameFn)(const symbol*, std::string&);
        
      public:
        symbol(string* name) : object(obj::O_SYMBOL), code(intern(name)->value()) {
        }

        // XXX:
        symbol(const char* name) : object(obj::O_SYMBOL), code(intern2(name)->value()) {
        }

        bool eq(const symbol* s) const {
          return this == s;
        }

        std::string& show(std::string& buf) const {
          const obj::string* name = find_symbol_name(this, buf);
          if(name == NULL)  {
            buf = "#<SYMBOL ";
            buf += util::Str::toString(code);
            buf += ">";
          }
          return buf;
        }

        int value() const { return code; }

        static object* read(std::istream& in) {
          int len = read_int(in);
          std::string buf;
          return new symbol(read_str(in, len, buf).c_str());
          /*
          object* o = read_object(in);
          assert(o->type() == obj::O_STRING);
          return new symbol((string*)o);
          */
        }

        static symbol* create_from_code(int code) { return new symbol(code); }

      private:
        symbol(int code) : object(obj::O_SYMBOL), code(code) {}
        
      public:
        static TableLookupFn table_lookup; // XXX:
        static TableLookupByNameFn table_lookup_by_name;
        static TableLookupByCodeFn table_lookup_by_code;
        static TableLookupByNameFn intern;
        static TableLookupByNameFn2 intern2;
        static SymbolNameFn find_symbol_name;
      private:
        int code;
      };
      symbol::TableLookupFn symbol::table_lookup = NULL;
      symbol::TableLookupByNameFn symbol::table_lookup_by_name = NULL;
      symbol::TableLookupByNameFn symbol::intern = NULL;
      symbol::TableLookupByNameFn2 symbol::intern2 = NULL;
      symbol::TableLookupByCodeFn symbol::table_lookup_by_code = NULL;
      symbol::SymbolNameFn symbol::find_symbol_name = NULL;

      // XXX: delete
      class quote : public object {
      public:
        quote(object* x) : object(obj::O_QUOTE), x(x) {
        }
        
        std::string& show(std::string& buf) const {
          std::string b;
          buf = "'";
          buf += x->show(b);
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

      // XXX:
      symbol NIL = *symbol::create_from_code(0);
      symbol TRUE = *symbol::create_from_code(1);
      
      bool is_nil(const object* o) { 
        if(o==&NIL)
          return true;
        return false;
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

        std::string& show(std::string& buf) const; 

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

        std::string& show(std::string& buf) const;
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
        
        std::string& show(std::string& buf) const{
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

        static const object* car(const list* x) {
          if(x->is_null())
            return &NIL;
          return x->u_cons.get_car();
        }
        
        static const object* cdr(const list* x) {
          if(x->is_null())
            return &NIL;
          return x->u_cons.get_cdr();          
        }

        static list* cdr_list(list* x) {
          // TODO: validation
          return to_list(cdr(x));
        }

        static const list* cdr_list(const list* x) {
          // TODO: validation
          return to_list(cdr(x));
        }
        
        static object* first(list* x) {
          return car(x);
        }

        static object* second(list* x) {
          return car(cdr_list(x));
        }

        static const object* first(const list* x) {
          return car(x);
        }

        static const object* second(const list* x) {
          return car(cdr_list(x));
        }

        static object* third(list* x) {
          return car(cdr_list(cdr_list(x)));
        }        

        static list* to_list(object* x) {
          return reinterpret_cast<list*>(x);
        }

        static const list* to_list(const object* x) {
          return reinterpret_cast<const list*>(x);
        }

        /*
        static bool eql(const list* x, const list* y) {
          if(x->length() != y->length())
            return false;
          
          X_LIST_EACH2(a, b, x, y, {
              if(!(*a == *b))
                return false;
            });
          
          return true;
        }
        */
      };

      int list::length() const {
        int len = 0;
        X_LIST_EACH(x, this, {
            assert(x); // XXX:
            len++;
          });
        return len;
      }
      object* list::read(std::istream& in) {
        list* head = lists::to_list(&NIL);
        int len = read_int(in);
        for(int i=0; i < len; i++) {
          head = lists::to_list(new cons(read_object(in), &head->u_obj));
        }
        return &lists::reverse(head)->u_obj;
      }   

      std::string& list::show(std::string& buf) const {
        if(length()==2 && lists::first(this) == symbol::intern2("QUOTE")) {
          std::string b;
          buf = "'";
          buf += lists::second(this)->show(b);
          return buf;
        } else {
          std::string b;
          buf = "(";
          
          X_LIST_EACH(x, this, {
              if(buf.size() != 1)
                buf += " ";
              buf += x->show(b);
            });
          
          buf += ")";
          return buf;
        }
      }

      std::string& cons::show(std::string& buf) const{
        if(is_proper_list()) 
          return lists::to_list(this)->show(buf);
        
        std::string b;
        buf = "(";
        
        const object* cur = this;
        for(; cur->type()==obj::O_CONS; cur = ((cons*)cur)->cdr) {
          if(buf.size() != 1)
            buf += " ";
          buf += ((cons*)cur)->car->show(b);
        }
        buf += " . ";
        buf += cur->show(b);

        buf += ")";
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

        string(list* src) : object(obj::O_STRING), head(src) {
          X_LIST_EACH(x, src, {
              assert(x->type() == obj::O_INTEGER);
            });
        }
        
        std::string& show(std::string& buf) const{
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

        bool operator==(const string& s) const {
          if(head->length() != s.head->length())
            return false;
          
          X_LIST_EACH2(a, b, head, s.head, {
              if(((integer*)a)->value() != ((integer*)b)->value())
                return false;
            });
          
          return true;
        }

        std::string& c_string(std::string& buf) const {
          buf.clear();
          X_LIST_EACH(c, head, {
              buf += ((obj::integer*)c)->value();
            });
          return buf;
        }

        list* to_list() const { return head; }

      private:
        list* head;
      };

      class function : public object {
      public:
        function(list* params, list* body, environment* e) 
          : object(obj::O_FUNCTION), params(params), body(body), env(e) {
          X_LIST_EACH(p, params, {
              if(p->type() != obj::O_SYMBOL)
                ERR(p->show() + " is not a symbol");
          });

          // add implicit progn
          this->body = lists::to_list(new cons(new special(special::PROGN), &body->u_obj));
        }

        std::string& show(std::string& buf) const{
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
        environment* get_env() const { return env; }

      protected:
        obj::list* params;
        obj::list* body;
        environment* env;
      };

      class macro_function : public function {
      public:
        macro_function(list* params, list* body, environment* e) 
          : function(params, body, e) {
          m_type = obj::O_MACRO_FUNCTION;
        }

        std::string& show(std::string& buf) const{
          std::string b;
          buf = "#<MACRO_FUNCTION ";
          buf += params->show(b);
          buf += " ";
          buf += body->show(b);
          buf += ">";
          return buf;
        }
      };

      class symbol_macro : public object {
      public:
        symbol_macro(object* exp) 
          : object(obj::O_SYMBOL_MACRO), exp(exp) {
        }

        std::string& show(std::string& buf) const{
          buf = "#<SYMBOL_MACRO ";
          buf += exp->show();
          buf += ">";
          return buf;
        }

        object* value() const { return exp; }

      private:
        object* exp;
      };

      class native_function : public object {
      public:
        native_function(int fn_index) 
          : object(obj::O_NATIVE_FUNCTION), fn_index(fn_index) {}
        
        obj::object* apply(NATIVE_FN* table, obj::list* args, environment* env) {
          return table[fn_index](args, env);
        }

        std::string& show(std::string& buf) const{
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

      // file-stream
      class stream : public object {
      public:
        stream(int fd) : object(obj::O_STREAM), fd(fd) {
        }

        std::string& show(std::string& buf) const{
          buf = "#<STREAM ";
          buf += util::to_string(fd);
          buf += ">";
          return buf;
        }
        
        int value() const { return fd; }
        
        static object* read(std::istream& in) {
          return new stream(read_int(in));
        }        
        
      private:
        int fd;
      };

      object* read_object(std::istream& in) {
        int type = read_int(in);
        switch(type) {
        case O_CONS: return cons::read(in);
        case O_LIST: return list::read(in);
        case O_STRING: return string::read(in);
        case O_REFER: return refer::read(in);
        case O_INTEGER: return integer::read(in);
        case O_STREAM: return stream::read(in);
        case O_OBJECT: return object::read(in);
        case O_SYMBOL: 
          {
            // for eq
            obj::symbol* o = (obj::symbol*)symbol::read(in);
            o = symbol::table_lookup(o);
            assert(o != NULL);
            return o;
          }
        case O_QUOTE: return quote::read(in);
        case O_SPECIAL: return special::read(in);
        case O_NATIVE_FUNCTION: return native_function::read(in);
        default:
          ERR(std::string("Unexpected type: ") + util::to_string(type));
        }
      }

      object* o_nil() { return &obj::NIL; }
      object* o_t() { return &obj::TRUE; }
      

      bool is_integer(object* o) { return o->type() == obj::O_INTEGER; }
      bool is_symbol(object* o) { return o->type() == obj::O_SYMBOL; }
      bool is_string(object* o) { return o->type() == obj::O_STRING; }
      bool is_stream(object* o) { return o->type() == obj::O_STREAM; }
      
      integer* to_integer(object* o) {
        assert(is_integer(o));
        return (integer*)o;
      }

      symbol* to_symbol(object* o) {
        assert(is_symbol(o));
        return (symbol*)o;
      }

      string* to_string(object* o) {
        assert(is_string(o));
        return (string*)o;
      }

      stream* to_stream(object* o) {
        assert(is_stream(o));
        return (stream*)o;
      }
    }
  }
}

#endif

#ifndef PSIL_CORE_UTIL_HH
#define PSIL_CORE_UTIL_HH

#include <string>
#include <sstream>
#include <iostream>

namespace psil {
  namespace core {
    namespace obj {
      class object;
      union list;
    }
    class environment;

    // XXX: move (type.hh?)
    typedef obj::object* (*NATIVE_FN) (obj::list*, environment*);

    namespace util {
      class Str {
      public:
        template <typename T>
        static std::string toString(const T& x) {
          std::stringstream ss;
          ss << x;
          return ss.str();
        }
      };

      void err(const std::string& message) {
        std::cerr << std::endl << "[ERROR] " << message << std::endl;
      }

      template <typename T>
      std::string to_string(const T& x) {
        return Str::toString(x);
      }
    }
  }
}

#define PC_OBJ psil::core::obj

#define X_LIST_EACH(var, list_val, exp)   \
  {\
    PC_OBJ::list* cur = (PC_OBJ::list*)(list_val);           \
    for(; cur->is_null()==false; cur = PC_OBJ::lists::cdr_list(cur)) { \
      PC_OBJ::object* var = PC_OBJ::lists::car(cur); \
      exp \
    } \
  }

#define X_LIST_EACH2(var1, var2, list_val1, list_val2, exp)  \
  {\
    PC_OBJ::list* cur1 = (PC_OBJ::list*)(list_val1);                     \
    PC_OBJ::list* cur2 = (PC_OBJ::list*)(list_val2);                     \
    for(; cur1->is_null()==false && cur2->is_null()==false;             \
        cur1 = PC_OBJ::lists::cdr_list(cur1), cur2 = PC_OBJ::lists::cdr_list(cur2)) { \
          PC_OBJ::object* var1 = PC_OBJ::lists::car(cur1);               \
          PC_OBJ::object* var2 = PC_OBJ::lists::car(cur2);              \
          exp                                                           \
    } \
  }

#define LIST_EACH(var, list_val, exp)   \
  {\
    PC_OBJ::object* cur = (list_val)->value();                        \
    for(; PC_OBJ::is_nil(cur)==false; cur = PC_OBJ::list::cdr(cur)) { \
      PC_OBJ::object* var = PC_OBJ::list::car(cur); \
      exp \
    } \
  }

#define LIST_EACH2(var1, var2, list_val1, list_val2, exp)  \
  {\
    PC_OBJ::object* cur1 = (list_val1)->value();        \
    PC_OBJ::object* cur2 = (list_val2)->value();                      \
    for(; PC_OBJ::is_nil(cur1)==false && PC_OBJ::is_nil(cur2)==false; \
          cur1 = PC_OBJ::list::cdr(cur1), cur2 = PC_OBJ::list::cdr(cur2)) { \
      PC_OBJ::object* var1 = PC_OBJ::list::car(cur1); \
      PC_OBJ::object* var2 = PC_OBJ::list::car(cur2); \
      exp \
    } \
  }

#define ERR(message) {psil::core::util::err(message); throw "<<abort>>";}

#endif

#ifndef PSIL_VM_RETURN_STACK_HH
#define PSIL_VM_RETURN_STACK_HH

#include "Object.hh"
#include <vector>
#include <string>

/*
  [layout]
  previous-base
  return-address
  context
*/
namespace psil {
  namespace vm {
    class BytecodeObject;
    typedef BytecodeObject Context;

    class ReturnStack {
    public:
      struct Entry {
        Entry(unsigned b, unsigned r, Context* cx)
          : base(b), returnAddress(r), context(cx) {}
        unsigned base;
        unsigned returnAddress;
        Context* context;
      };

    public:
      void push(const Entry& e) {
        stack.push_back(e);
      }
      
      Entry pop() {
        const Entry& e = stack.back();
        stack.pop_back();
        return e;
      }

      std::string show() const;

    private:
      std::vector<Entry> stack;
    };
  }
}

#endif

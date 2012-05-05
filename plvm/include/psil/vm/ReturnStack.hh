#ifndef PSIL_VM_RETURN_STACK_HH
#define PSIL_VM_RETURN_STACK_HH

#include "Object.hh"
#include <vector>
#include <string>

/*
  [layout]
  previous-top
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
        Entry(unsigned t, unsigned b, unsigned r, Context* cx)
          : top(t), base(b), returnAddress(r), context(cx) {}
        unsigned top;
        unsigned base;
        unsigned returnAddress;
        Context* context;
      };

    public:
      void clear() {
        stack.clear();
      }

      void push(const Entry& e) {
        stack.push_back(e);
      }
      
      Entry pop() {
        const Entry& e = stack.back();
        stack.pop_back();
        return e;
      }

      bool isEmpty() const {
        return stack.empty();
      }

      std::string show() const;

    private:
      std::vector<Entry> stack;
    };
  }
}

#endif

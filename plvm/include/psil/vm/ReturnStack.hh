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
*/
namespace psil {
  namespace vm {
    class ReturnStack {
    public:
      struct Entry {
        Entry(unsigned t, unsigned b, unsigned r) : top(t), base(b), returnAddress(r) {}
        unsigned top;
        unsigned base;
        unsigned returnAddress;
      };

    public:
      void push(const Entry& e) {
        stack.push_back(e);
      }
      
      Entry pop() {
        const Entry& e = stack.front();
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

#ifndef PSIL_VM_DATA_STACK_HH
#define PSIL_VM_DATA_STACK_HH

#include "Object.hh"
#include <vector>
#include <string>

/*
  [stack-layout]
  -- top --
  tmp-x2
  tmp-x1
  local-var2
  local-var1
  close2
  close1
  arg2
  arg1
  -- base --
 */
namespace psil {
  namespace vm {
    class DataStack {
    public:
      DataStack() : base(0) {} 
      
      void clear() {
        base = 0;
        stack.clear();
      }
      
      void push(type::Object* o) { stack.push_back(o); }
      type::Object* pop() { type::Object* v=stack.back(); stack.pop_back(); return v; }
      type::Object* front() { return stack.back(); };
      void reserve(unsigned count) { stack.resize(stack.size()+count); }
      void drop(unsigned count) { stack.resize(stack.size()-count); }
      void reset(unsigned base, unsigned top) { 
        this->base = base;
        stack.resize(top);
      }
      void erase(unsigned start, unsigned end) {
        stack.erase(stack.begin()+start, stack.begin()+end);
      }
      
      type::Object* localGet(unsigned index) { return stack[base+index]; }
      void localSet(unsigned index, type::Object* value) { stack[base+index] = value; }

      std::string show() const;

      unsigned getTop() const { return stack.size(); }
      unsigned getBase() const { return base; }
      void setTop(unsigned top) { stack.resize(top); }
      void setBase(unsigned base) { this->base = base; }
    private:
      unsigned base;
      std::vector<type::Object*> stack;
    };
  }
}

#endif

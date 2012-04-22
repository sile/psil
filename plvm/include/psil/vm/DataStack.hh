#ifndef PSIL_VM_DATA_STACK_HH
#define PSIL_VM_DATA_STACK_HH

#include "Object.hh"
#include <vector>
#include <string>

/*
  [stack-layout]
  -- top --
  x2
  x1
  local-var2
  local-var1
  -- base --
  arg1
  arg2

  [in heap]
  closed-var1
  closed-var2
 */
namespace psil {
  namespace vm {
    class DataStack {
    public:
      DataStack() : base(0) {} 
      
      void push(type::Object* o) { stack.push_back(o); }
      type::Object* pop() { type::Object* v=stack.back(); stack.pop_back(); return v; }
      void reserve(unsigned count) { stack.resize(stack.size()+count); }
      void drop(unsigned count) { stack.resize(stack.size()-count); }
      void reset(unsigned base, unsigned top) { 
        this->base = base;
        stack.resize(top);
      }
      
      type::Object* arg_get(unsigned index) { return stack[base-index-1]; }
      void arg_set(unsigned index, type::Object* value) { stack[base-index-1] = value; }
      type::Object* local_get(unsigned index) { return stack[base+index]; }
      void local_set(unsigned index, type::Object* value) { stack[base+index] = value; }

      std::string show() const;

    private:
      unsigned base;
      std::vector<type::Object*> stack;
    };
  }
}

#endif

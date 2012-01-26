#ifndef PVM_HH
#define PVM_HH

#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <algorithm>

namespace pvm {
  typedef unsigned char octet;
  typedef std::vector<int> stack_t;

  // bytecode_stream
  class bytecode_stream {
  public:
    bytecode_stream(const char* filepath) : bytecodes(NULL), position(0) {
      std::ifstream in(filepath);
      assert(in);

      length = in.rdbuf()->in_avail();
      bytecodes = new octet[length];
      in.read((char*)bytecodes, length);
    }
    
    ~bytecode_stream() {
      delete [] bytecodes;
    }
    
    bool eos() const { return position >= length; }
    
    octet read_octet () {
      return bytecodes[position++];
    }

    // sizeof(int) == 4 と仮定
    int read_int() {
      int n = *(int*)(bytecodes + position);
      position += 4;
      return n;
    }

    // program counter
    unsigned pc() const { return position; }
    unsigned& pc() { return position; }
    
  private:
    octet* bytecodes;
    unsigned length;
    unsigned position;
  };

  class environment {
  public:
    stack_t& dstack() { return data_stack; }
    stack_t& rstack() { return return_stack; }

    const stack_t& dstack() const { return data_stack; }
    const stack_t& rstack() const { return return_stack; }

  private:
    stack_t data_stack;
    stack_t return_stack;
  };

  class op {
  public:
    static void call(octet opcode, bytecode_stream& in, environment& env) {
      switch(opcode) {
      case  1: op_int(in, env); break; // read int value
      case  2: op_add(in, env); break; // +
      case  3: op_sub(in, env); break; // -
      case  4: op_mul(in, env); break; // *
      case  5: op_div(in, env); break; // /
      case  6: op_mod(in, env); break; // %
      case  7: op_eql(in, env); break; // ==
      case  8: op_less(in, env); break;// <

      case  9: op_dup(in, env); break; // duplicates head of data stack
      case 10: op_drop(in, env); break; // drop head of data stack
      case 11: op_swap(in, env); break; // swap first and second of data stack
      case 12: op_over(in, env); break; // copy second of data stack and push to front
      case 13: op_rot(in, env); break;  // rotate third to first
        
      case 14: op_rpush(in, env); break; // 
      case 15: op_rpop(in, env); break; //
      case 16: op_rcopy(in, env); break; //

      case 17: op_jump(in, env); break;
      case 18: op_jump_if(in, env); break;
      case 19: op_call(in, env); break;
      case 20: op_return(in, env); break;
        
      default:
        assert(false);
      }
    }

  private:
    typedef bytecode_stream bcs;
    typedef environment env;
    
#define DPUSH(x) e.dstack().push_back(x)
#define DPOP pop_back(e.dstack())
#define DNTH(nth) e.dstack()[e.dstack().size()-1-nth]

#define RPUSH(x) e.rstack().push_back(x)
#define RPOP pop_back(e.rstack())
#define RNTH(nth) e.rstack()[e.rstack().size()-1-nth]

    static void op_int(bcs& in, env& e) { DPUSH(in.read_int()); }
    static void op_add(bcs& in, env& e) { DPUSH(DPOP + DPOP); }
    static void op_sub(bcs& in, env& e) { int n = DPOP; DPUSH(DPOP - n); }
    static void op_mul(bcs& in, env& e) { DPUSH(DPOP * DPOP); }
    static void op_div(bcs& in, env& e) { int n = DPOP; DPUSH(DPOP / n); }
    static void op_mod(bcs& in, env& e) { int n = DPOP; DPUSH(DPOP % n); }
    static void op_eql(bcs& in, env& e) { DPUSH(DPOP == DPOP); }
    static void op_less(bcs& in, env& e) { DPUSH(DPOP > DPOP); }

    static void op_dup(bcs& in, env& e) { DPUSH(DNTH(0)); }
    static void op_drop(bcs& in, env& e) { DPOP; }
    static void op_swap(bcs& in, env& e) { std::swap(DNTH(0), DNTH(1)); }
    static void op_over(bcs& in, env& e) { DPUSH(DNTH(1)); }
    static void op_rot(bcs& in, env& e) { std::swap(DNTH(2), DNTH(0)); 
                                          std::swap(DNTH(1), DNTH(2)); }

    static void op_rpush(bcs& in, env& e) { RPUSH(DPOP); }
    static void op_rpop(bcs& in, env& e) { DPUSH(RPOP); }
    static void op_rcopy(bcs& in, env& e) { DPUSH(RNTH(0)); }

    static void op_jump(bcs& in, env& e) { in.pc() = DPOP;}
    static void op_jump_if(bcs& in, env& e) { int p = DPOP; if(DPOP){ in.pc() = p;} }
    static void op_call(bcs& in, env& e) { RPUSH(in.pc()); in.pc() = DPOP; }
    static void op_return(bcs& in, env& e) { in.pc() = RPOP; }

#undef DPUSH
#undef DPOP
#undef DNTH

#undef RPUSH
#undef RPOP
#undef RNTH

  private:
    static int pop_back(stack_t& stack) {
      int x = stack.back();
      stack.pop_back();
      return x;
    }
  };

  // executor
  class executor {
  public:
    void execute(const char* filepath) {
      bytecode_stream in(filepath);
      
      while(in.eos() == false) {
        octet opcode = in.read_octet();
        op::call(opcode, in, env);
      }
    }
    
    const environment& get_env() const { return env; }

  private:
    environment env;
  };
}

#endif

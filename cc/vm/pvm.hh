#ifndef PVM_HH
#define PVM_HH

#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <algorithm>

namespace pvm {
  typedef unsigned char octet;

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
    std::vector<int>& dstack() { return data_stack; }
    std::vector<int>& rstack() { return return_stack; }

    const std::vector<int>& dstack() const { return data_stack; }
    const std::vector<int>& rstack() const { return return_stack; }

  private:
    std::vector<int> data_stack;
    std::vector<int> return_stack;
  };

  typedef void (*OPFUN)(bytecode_stream&, environment&);
  class op {
  public:
    static OPFUN get_op(octet opcode) {
      switch(opcode) {
      case  1: return op_int; // read int value
      case  2: return op_add; // +
      case  3: return op_sub; // -
      case  4: return op_mul; // *
      case  5: return op_div; // /
      case  6: return op_mod; // %
      case  7: return op_eql; // ==
      case  8: return op_less;// <

      case  9: return op_dup; // duplicates head of data stack
      case 10: return op_drop; // drop head of data stack
      case 11: return op_swap; // swap first and second of data stack
      case 12: return op_over; // copy second of data stack and push to front
      case 13: return op_rot;  // rotate third to first
        
      case 14: return op_rpush; // 
      case 15: return op_rpop; //
      case 16: return op_rcopy; //

      case 17: return op_jump;
      case 18: return op_jump_if;
      case 19: return op_call;
      case 20: return op_return;
      }
      assert(false);
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
    static void op_sub(bcs& in, env& e) { DPUSH(- DPOP + DPOP); }
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
    static int pop_back(std::vector<int>& stack) {
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
        op::get_op(opcode)(in, env);
      }
    }
    
    const environment& get_env() const { return env; }

  private:
    environment env;
  };
}

#endif

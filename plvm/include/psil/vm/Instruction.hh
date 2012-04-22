#ifndef PSIL_VM_INSTRUCTION_HH
#define PSIL_VM_INSTRUCTION_HH

#include "type.hh"
#include "Environment.hh"
#include "Object.hh"
#include "DataStack.hh"
#include "ReturnStack.hh"
#include <cassert>
#include <iostream>

namespace psil {
  namespace vm {
    using namespace type;

    class Instruction {
    public:
      Instruction(Environment& env) : env(env) {};
      
      bool hasNextOp() const {
        return env.getCodeStream().eos() == false;
      }

      void execute() {
        opcode_t op =  env.getCodeStream().readUint1();
        
        switch (op) {
        // => 最終的にはこの辺りは全部定数テーブルに移動した方が良いかも
        case   1: _int(); break;
        case   2: _string(); break;
        case   3: _char(); break;
        case   4: _symbol(); break;
        case   5: _nil(); break;
        case   6: _true(); break;
        case   7: _false(); break;
        case   8: _list(); break;
                                                
        case  50: _symget(); break;
        case  51: _symset(); break;
        case  52: _constget(); break;
          
        case 101: _apply(); break;
        case 102: _tail_apply(); break;
        case 103: _return(); break;
        case 104: _conti(); break;
        case 105: _nuate(); break;
        case 106: _recur_tail_apply(); break;

        case 150: _jump(); break;
        case 151: _jump_if(); break;
        case 152: _fix_jump(); break;
        case 153: _fix_jump_if(); break;

        case 180: _drop(); break;
        case 181: _dropn(); break;
          
        case 201: _lambda(); break;
        case 202: _localget(); break;
        case 203: _localset(); break;
        case 204: _reference(); break;
        case 205: _refget(); break;
        case 206: _refset(); break;
        case 207: _argget(); break;
        case 208: _argset(); break;
          
        case 250: _print(); break;
          
        default:
          assert(false);
        }
      }

    private:
      Environment& env;
      
    private:
      // TODO: 全体的にpop => push系の命令は最適化できる

      void _int() {
        push(Int::make(readUint4()));
      }

      void _string() {
        uint4 length = readUint4();
        std::string str;
        env.getCodeStream().readString(str, length);
        push(String::make(str));
      }

      void _char() {
        push(Char::make(readUint4()));
      }

      void _symbol() {
        uint2 length = readUint2();
        std::string str;
        env.getCodeStream().readString(str, length);
        push(Symbol::make(str));
      }

      void _nil() {
        push(Nil::make());
      }

      void _true() {
        push(Boolean::make(true));
      }
      
      void _false() {
        push(Boolean::make(false));
      }
      
      void _list() {
        uint4 length = readUint4();
        Object* head = Nil::make();
        for(uint4 i=0; i < length; i++) {
          head = Cons::make(pop(), head);
        }
        push(head);
      }

      void _symget() {
        push(to<Symbol>(pop())->getValue());
      }
      void _symset() {
        push(to<Symbol>(pop())->setValue(pop()));
      }
      void _constget() {
        push(env.getConstantTable().get(readUint2()));
      }

      //
      void _jump() {
        int4 offset = to<Int>(pop())->getValue();
        env.getCodeStream().jump(offset);
      }

      void _jump_if() {
        int4 offset = to<Int>(pop())->getValue();
        if(! Boolean::isFalse(pop())) {
          env.getCodeStream().jump(offset);          
        }
      }

      void _fix_jump() {
        env.getCodeStream().jump(readInt2());
      }

      void _fix_jump_if() {
        int2 offset = readInt2();
        if(! Boolean::isFalse(pop())) {
          env.getCodeStream().jump(offset);
        }
      }

      //
      void _drop() {
        pop();
      }

      void _dropn() {
        env.getDataStack().drop(readUint1());
      }
      
      void _apply() {
        Object* o = pop();
        if(o->getType() == TYPE_LAMBDA) {
          Lambda& lambda = *to<Lambda>(pop());
          create_callframe(lambda);
          env.restoreContext(lambda.getContext(), lambda.getBodyAddress());
        } else {
          NativeLambda& lambda = *to<NativeLambda>(pop());
          lambda.getBody()(env);
        }
      }

      void _tail_apply() {
        Object* o = pop();
        if(o->getType() == TYPE_LAMBDA) {
          Lambda& lambda = *to<Lambda>(pop());
          create_tail_callframe(lambda);
          env.restoreContext(lambda.getContext(), lambda.getBodyAddress());
        } else {
          NativeLambda& lambda = *to<NativeLambda>(pop());
          lambda.getBody()(env);
        }
      }

      void _recur_tail_apply() {
        // TODO:
        assert(false);
      }
      
      void _return() {
        DataStack& ds = env.getDataStack();
        ReturnStack& rs = env.getReturnStack(); 
        ReturnStack::Entry e = rs.pop();
        Object* returnValue = pop();

        ds.setTop(e.top);
        ds.setBase(e.base);
        env.restoreContext(e.context, e.returnAddress);
        push(returnValue);
      }

      void _conti() {
        // TODO:
      }

      void _nuate() {
        // TODO:
      }

      // 
      void _lambda() {
        uint1 closed_val_count = readUint1();
        uint1 arity = readUint1();
        uint1 local_var_count = readUint1();
        unsigned body_size = readUint4();
        
        Object** closed_vals = closed_val_count==0 ? NULL : new Object*[closed_val_count];
        for(uint1 i=0; i < closed_val_count; i++) {
          closed_vals[i] = pop();
        }
        
        Lambda* lambda = 
          Lambda::make(closed_vals, closed_val_count, arity, local_var_count, 
                       env.getCodeStream().getPosition(),
                       env.getContext());

        env.getCodeStream().jump(body_size);
        push(lambda);
      }

      void _localget() {
        push(env.getDataStack().local_get(readUint1()));
      }

      void _localset() {
        push(env.getDataStack().local_set(readUint1(), pop()));
      }

      void _argget() {
        push(env.getDataStack().arg_get(readUint1()));
      }

      void _argset() {
        push(env.getDataStack().arg_set(readUint1(), pop()));
      }

      void _reference() {
        push(Reference::make(pop()));
      }

      void _refget() {
        push(to<Reference>(pop())->getValue());
      }

      void _refset() {
        push(to<Reference>(pop())->setValue(pop()));        
      }

      // 
      void _print() {
        std::cerr << env.getDataStack().front()->show() << std::endl;
      }

    private:
      void create_callframe(Lambda& lambda) {
        for(uint1 i=0; i < lambda.getClosedValueCount(); i++) {
          push(lambda.getClosedValue(i));
        }
        DataStack& ds = env.getDataStack();
        ReturnStack& rs = env.getReturnStack();
        ReturnStack::Entry e(ds.getTop() - lambda.getArity(), 
                             ds.getBase(),
                             env.getCodeStream().getPosition(), // TODO: saveContextとか用意
                             env.getContext());
        rs.push(e);
        
        ds.setBase(ds.getTop());
        ds.reserve(lambda.getLocalVarCount());
      }

      void create_tail_callframe(Lambda& lambda) {
        for(uint1 i=0; i < lambda.getClosedValueCount(); i++) {
          push(lambda.getClosedValue(i));
        }
        DataStack& ds = env.getDataStack();
        ds.setBase(ds.getTop());
        ds.reserve(lambda.getLocalVarCount());
      }

      opcode_t readOp() {
        return env.getCodeStream().readUint4();
      }
      
      uint4 readUint4() {
        return env.getCodeStream().readUint4();
      }

      uint2 readUint2() {
        return env.getCodeStream().readUint2();
      }

      int2 readInt2() {
        return (int2)env.getCodeStream().readUint2();
      }

      uint1 readUint1() {
        return env.getCodeStream().readUint1();
      }
      
      void push(Object* x) {
        env.getDataStack().push(x);
      }

      Object* pop() {
        return env.getDataStack().pop();
      }
    };
  }
}

#endif

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
        // => 最終的にはこの辺り(定数生成系)は全部定数テーブルに移動した方が良いかも
        case   1: _int(); break;
        case   2: _string(); break;
        case   3: _char(); break;
        case   4: _symbol(); break;
        case   5: _nil(); break;
        case   6: _true(); break;
        case   7: _false(); break;
        case   8: _list(); break;
        case   9: _undef(); break;
                                                
        case  50: _symget(); break;
        case  51: _symset(); break;
        case  52: _constget(); break;
          
        case 101: _apply(); break;
        case 102: _tail_apply(); break;
        case 103: _return(); break;
        case 104: _conti(); break;
        case 105: _nuate(); break;
        case 106: _recur_tail_apply(); break;
        case 107: _list_apply(); break;

        case 130: _eval(); break;

        case 150: _jump(); break;
        case 151: _jump_if(); break;
        case 152: _fix_jump(); break;
        case 153: _fix_jump_if(); break;

        case 180: _drop(); break;
        case 181: _dropn(); break;
          
        case 201: _lambda(); break;
        case 202: _localget(); break;
        case 203: _localset(); break;
        case 204: _local_mkref();  break;
        case 205: _local_refget(); break;
        case 206: _local_refset(); break;
        case 207: _local_toref(); break;
        case 208: _reserve(); break;
          
        case 250: _print(); break;
        case 251: _show_stack(); break;
          
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

      void _undef() {
        push(Undef::make());
      }

      void _symget() {
        Symbol* sym = to<Symbol>(pop());
        // std::cerr << "# " << sym->getName() << ": " << sym->getValue()->getType() << std::endl;
        push(sym->getValue());
        //push(to<Symbol>(pop())->getValue());
      }
      void _symset() {
        Object* sym = pop();
        to<Symbol>(sym)->setValue(pop());
      }
      void _constget() {
        push(env.getConstantTable().get(readUint2()));
      }
      
      //
      void _eval() {
        // XXX: 空リストは来ないと仮定
        Cons* list = to<Cons>(pop());

        int len = 1;
        Cons* head = list;
        while(head->getCdr() != Nil::make()) {
          head = to<Cons>(head->getCdr());
          len++;
        }
        
        char* bytes = new char[len + 1];
        head = list;
        bytes[0] = (char)to<Int>(head->getCar())->getValue();
        for(int i=1; i < len; i++) {
          head = to<Cons>(head->getCdr());
          bytes[i] = (char)to<Int>(head->getCar())->getValue();
        }
        bytes[len] = 103; // return

        create_eval_callframe();

        BytecodeObject::appendRuntimeCode(bytes, len + 1);
        env.restoreContext(&BytecodeObject::getRuntime(), 
                           BytecodeObject::getRuntime().getCodeStream().getPosition());
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

      void _list_apply() {
        Object* fn = pop();
        Object* head = pop();
        uint1 arity = 0;
        while(head != Nil::make()) {
          Cons* cons = to<Cons>(head);
          push(cons->getCar());
          head = cons->getCdr();
          arity++;
        }
        apply_impl(fn, false, arity);
      }      

      void _apply() {
        Object* fn = pop();
        uint1 arity = readUint1();
        apply_impl(fn, false, arity);
      }

      void _tail_apply() {
        Object* fn = pop();
        uint1 arity = readUint1();
        apply_impl(fn, true, arity);
      }

      void apply_impl(Object* o, bool isTailCall, uint1 arity) {
        if(o->getType() == TYPE_LAMBDA) {
          Lambda& lambda = *to<Lambda>(o);
          if(lambda.isVarArg()) {
            assert(lambda.getArity() <= arity+1);
            Object* head = Nil::make();
            for(uint1 i=lambda.getArity(); i < arity + 1; i++) {
              head = Cons::make(pop(), head);
            }
            push(head);
          } else {
            assert(lambda.getArity() == arity);
          }

          if(isTailCall && 
             // TODO: あらかじめ番兵値を入れておいて、このチェックは不要にする
             env.getReturnStack().isEmpty() == false) { 
            create_tail_callframe(lambda);
          } else {
            create_callframe(lambda);
          }
          env.restoreContext(lambda.getContext(), lambda.getBodyAddress());
        } else {
          NativeLambda& lambda = *to<NativeLambda>(o);
          lambda.getBody()(env, arity);
        } 
      }
      
      void _recur_tail_apply() {
        assert(false);
        /*
        uint1 arity = readUint1();
        Object* o = pop();
        if(o->getType() == TYPE_LAMBDA) {
          Lambda& lambda = *to<Lambda>(o);
          create_recur_tail_callframe(lambda);
          env.restoreContext(lambda.getContext(), lambda.getBodyAddress());
        } else {
          NativeLambda& lambda = *to<NativeLambda>(o);
          lambda.getBody()(env);
        }
        */
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
        bool vararg = readUint1() != 0;
        unsigned body_size = readUint4();
        
        Object** closed_vals = closed_val_count==0 ? NULL : new Object*[closed_val_count];
        for(uint1 i=0; i < closed_val_count; i++) {
          closed_vals[i] = pop();
        }
        
        Lambda* lambda = 
          Lambda::make(closed_vals, closed_val_count, arity, local_var_count, 
                       env.getCodeStream().getPosition(),
                       env.getContext(), vararg);

        env.getCodeStream().jump(body_size);
        push(lambda);
      }

      void _localget() {
        push(env.getDataStack().localGet(readUint1()));
      }

      void _localset() {
        env.getDataStack().localSet(readUint1(), pop());
      }

      void _local_mkref() {
        env.getDataStack().localSet(readUint1(), Reference::make(pop()));
      }

      void _local_refget() {
        push(to<Reference>(env.getDataStack().localGet(readUint1()))->getValue());
      }

      void _local_refset() {
        Reference* ref = to<Reference>(env.getDataStack().localGet(readUint1()));
        ref->setValue(pop());
      }

      void _local_toref() {
        Object* o = env.getDataStack().localGet(readUint1());
        env.getDataStack().localSet(readUint1(), Reference::make(o));
      }

      void _reserve() {
        env.getDataStack().reserve(readUint1());
      }

      // 
      void _print() {
        std::cerr << env.getDataStack().front()->show() << std::endl;
      }

      void _show_stack() {
        DataStack& ds = env.getDataStack();
        ReturnStack& rs = env.getReturnStack(); 
        std::cerr << "###################" << std::endl;
        std::cerr << ds.show() << std::endl;
        std::cerr << rs.show() << std::endl;
        std::cerr << "###################" << std::endl;
      }

    private:
      void create_eval_callframe() {
        DataStack& ds = env.getDataStack();
        ReturnStack& rs = env.getReturnStack();
        unsigned nextBase = ds.getTop();
        
        ReturnStack::Entry e(nextBase,
                             ds.getBase(),
                             env.getCodeStream().getPosition(), // TODO: saveContextとか用意
                             env.getContext());
        rs.push(e);
        ds.setBase(nextBase);
      }

      void create_callframe(Lambda& lambda) {
        DataStack& ds = env.getDataStack();
        ReturnStack& rs = env.getReturnStack();
        unsigned nextBase = ds.getTop() - lambda.getArity();

        for(uint1 i=0; i < lambda.getClosedValueCount(); i++) {
          push(lambda.getClosedValue(i));
        }
        
        ReturnStack::Entry e(nextBase,
                             ds.getBase(),
                             env.getCodeStream().getPosition(), // TODO: saveContextとか用意
                             env.getContext());
        rs.push(e);
        
        ds.setBase(nextBase);
        ds.reserve(lambda.getLocalVarCount());
        /*
        for(uint1 i=0; i < lambda.getLocalVarCount(); i++) {
          push(Undef::make());
        }
        */
      }

      void create_tail_callframe(Lambda& lambda) {
        DataStack& ds = env.getDataStack();
        ds.erase(ds.getBase(), ds.getTop() - lambda.getArity());
        for(uint1 i=0; i < lambda.getClosedValueCount(); i++) {
          push(lambda.getClosedValue(i));
        }
        ds.reserve(lambda.getLocalVarCount());
      }

      void create_recur_tail_callframe(Lambda& lambda) {
        // TODO: 引数を上書き => topデクリメント
        assert(false);
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

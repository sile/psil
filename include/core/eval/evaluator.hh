#ifndef PSIL_CORE_EVAL_EVALUATOR_HH
#define PSIL_CORE_EVAL_EVALUATOR_HH

#include "../token/token.hh"

namespace psil {
namespace core {
namespace eval {
  typedef token::Token Value; // XXX:

  class Evaluator {
  public:
    Value* eval(const token::Token& token);
  };
}
}
}

#endif

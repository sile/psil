#include "core/psil_core.hpp"

using namespace psil::core::token;

int main(int argc, char** argv) {
  Token* tknInt = new TokenInt;

  return reinterpret_cast<long>(tknInt);
}

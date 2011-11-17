#include "core/psil_core.hh"

using namespace psil::core::token;

int main(int argc, char** argv) {
  TokenInt* tknInt = new TokenInt(10);

  return tknInt->value();
}

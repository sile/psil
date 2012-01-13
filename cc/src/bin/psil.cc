#include "core/psil.hh"
#include <iostream>

int main(int argc, char** argv) {
  psil::core::interpreter intr;
  intr.interpret(std::cin);
  return 0;
}

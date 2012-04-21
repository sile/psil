#include "psil/vm/BytecodeObject.hh"
#include <sstream>

using namespace psil::vm;

std::string BytecodeObject::show() const {
  std::ostringstream out;
  out << "[BytecodeObject]" << std::endl 
      << " header:" << std::endl
      << "  - version: " << header.version << std::endl
      << "  - constant_start: " << header.constant_start << std::endl
      << "  - constant_count: " << header.constant_count << std::endl
      << "  - code_start: " << header.code_start << std::endl
      << "  - code_size: " << header.code_size << std::endl
      << " constant-table: " << std::endl
      << const_table.show() << std::endl;
  return out.str();
}

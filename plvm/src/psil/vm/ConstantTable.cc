#include "psil/vm/type.hh"
#include "psil/vm/ConstantTable.hh"
#include <sstream>

using namespace psil::vm;

ConstantTable::ConstantTable(ByteStream& in, unsigned count) {
  for(unsigned i=0; i < count; i++) {
    uint2 length = in.readUint2();
    std::string name;
    in.readString(name, length);
    table.push_back(type::Symbol::intern(name));
  }
}

std::string ConstantTable::show() const {
  std::ostringstream out;
  out << "[ConstantTable]" << std::endl;
  for(std::size_t i=0; i < table.size(); i++) {
    out << " " << i << ": " << table[i]->show() << std::endl;
  }
  return out.str();
}

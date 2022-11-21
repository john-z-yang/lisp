#include "ParseException.hpp"
#include <ostream>
#include <string>

ParseException::ParseException(const std::string &msg, const std::string line,
                               const std::string::size_type charPos)
    : _msg(msg), line(line), charPos(charPos) {}

const char *ParseException::what() const noexcept { return _msg.c_str(); }

std::ostream &operator<<(std::ostream &o, const ParseException &pe) {
  o << "  " << pe.line << std::endl
    << "  " << std::string(pe.charPos, ' ') << "^" << std::endl
    << "Parse error: " << pe.what() << std::endl;
  return o;
}
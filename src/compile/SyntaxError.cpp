#include "SyntaxError.hpp"
#include <ostream>
#include <string>

SyntaxError::SyntaxError(const std::string &msg, const std::string line,
                         const std::string::size_type charPos)
    : _msg(msg), line(line), charPos(charPos) {}

const char *SyntaxError::what() const noexcept { return _msg.c_str(); }

std::ostream &operator<<(std::ostream &o, const SyntaxError &pe) {
  o << "  " << pe.line << std::endl
    << "  " << std::string(pe.charPos, ' ') << "^" << std::endl
    << "Parse error: " << pe.what() << std::endl;
  return o;
}
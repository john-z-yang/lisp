#include "SyntaxError.hpp"
#include <iomanip>
#include <ostream>
#include <string>

using namespace error;

SyntaxError::SyntaxError(
    const std::string msg,
    const std::string line,
    const unsigned int row,
    const unsigned int col
)
    : _msg(msg), line(line), row(row), col(col) {}

const char *SyntaxError::what() const noexcept { return _msg.c_str(); }

std::ostream &error::operator<<(std::ostream &o, const SyntaxError &se) {
  o << "Syntax error: " << se.what() << std::endl
    << "In line " << unsigned(se.row) << ":" << std::endl
    << std::setw(4) << "" << se.line << std::endl
    << std::setw(4) << "" << std::setw(se.col) << ""
    << "^" << std::endl;
  return o;
}

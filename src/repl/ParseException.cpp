#include "ParseException.hpp"
#include <ostream>
#include <string>

using std::endl;
using std::ostream;
using std::string;

ParseException::ParseException(const string &msg, const string line,
                               const string::size_type pos)
    : _msg(msg), line(line), pos(pos) {}

const char *ParseException::what() const noexcept { return _msg.c_str(); }

ostream &operator<<(ostream &o, const ParseException &pe) {
  o << "  " << pe.line << endl << "  " << string(pe.pos, ' ') << "^" << endl;
  o << "Parse error: " << pe.what() << endl;
  return o;
}
#include "StringAtom.hpp"
#include "cast.cpp"
#include <regex>
#include <string>

using std::regex_replace;
using std::string;
using std::to_string;

StringAtom::StringAtom(const string literal)
    : Atom(SExpr::Type::STR), literal(literal), unescaped(unescape(literal)) {}

string StringAtom::toString() const { return literal; }

string StringAtom::unescape(const string literal) {
  string res = literal;
  res = regex_replace(res, std::regex("\\\\\""), "\"");
  res = regex_replace(res, std::regex("\\\\\\\\"), "\\");
  return res.substr(1, res.size() - 2);
}

bool StringAtom::equals(const SExpr &other) const {
  if (isa<StringAtom>(other)) {
    return literal == dynamic_cast<const StringAtom &>(other).literal;
  }
  return false;
}

bool StringAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::STR;
}

const string StringAtom::typeName = "String";
#include "StringAtom.hpp"
#include "cast.cpp"
#include <regex>
#include <string>

StringAtom::StringAtom(const std::string literal)
    : Atom(SExpr::Type::STR), literal(literal), unescaped(unescape(literal)) {}

std::string StringAtom::toString() const { return literal; }

std::string StringAtom::unescape(const std::string literal) {
  auto res = literal;
  res = std::regex_replace(res, std::regex("\\\\\""), "\"");
  res = std::regex_replace(res, std::regex("\\\\\\\\"), "\\");
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

const std::string StringAtom::typeName = "String";
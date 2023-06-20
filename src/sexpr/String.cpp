#include "String.hpp"
#include "cast.cpp"
#include <regex>
#include <string>

using namespace sexpr;

String::String(const std::string literal)
    : Atom(SExpr::Type::STR), literal(literal), unescaped(unescape(literal)) {}

std::string String::toString() const { return literal; }

std::string String::unescape(const std::string literal) {
  auto res = literal;
  res = std::regex_replace(res, std::regex("\\\\\""), "\"");
  res = std::regex_replace(res, std::regex("\\\\\\\\"), "\\");
  return res.substr(1, res.size() - 2);
}

bool String::equals(const SExpr &other) const {
  if (isa<String>(other)) {
    return literal == cast<String>(other).literal;
  }
  return false;
}

bool String::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::STR;
}

const std::string String::typeName = "<String>";
#include "String.hpp"
#include "Casting.hpp"
#include <regex>
#include <string>

using namespace sexpr;

String::ValueType String::escape(const String::ValueType literal) {
  auto res = literal;
  res = std::regex_replace(res, std::regex("\\\\\""), "\"");
  res = std::regex_replace(res, std::regex("\\\\\\\\"), "\\");
  return res.substr(1, res.size() - 2);
}

std::ostream &String::serialize(std::ostream &o) const { return o << val; }

bool String::equals(const SExpr &other) const {
  if (const auto string = dynCast<String>(other)) {
    return val == string->get().val;
  }
  return false;
}

String::String(const String::ValueType literal)
    : Atom(SExpr::Type::STR), val(literal), escaped(escape(literal)) {}

bool String::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::STR;
}

std::string String::getTypeName() { return "<String>"; }

#include "BreakTable.hpp"

using namespace sexpr;
using namespace runtime;

SExpr *BreakTable::get(SExpr *const prevAddr) const {
  const auto it = addrs.find(prevAddr);
  if (it == addrs.end()) {
    return prevAddr;
  }
  return it->second;
}

void BreakTable::insert(SExpr *const prevAddr, SExpr *const curAddr) {
  addrs.insert(std::make_pair(prevAddr, curAddr));
}

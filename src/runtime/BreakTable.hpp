#ifndef LISP_SRC_RUNTIME_BREAKTABLE_HPP_
#define LISP_SRC_RUNTIME_BREAKTABLE_HPP_

#include <unordered_map>

namespace sexpr {
class SExpr;
}

namespace runtime {

class BreakTable {

private:
  std::unordered_map<sexpr::SExpr *, sexpr::SExpr *> addrs;

public:
  sexpr::SExpr *get(sexpr::SExpr *const prevAddr) const;
  void insert(sexpr::SExpr *const prevAddr, sexpr::SExpr *const curAddr);
};

} // namespace runtime

#endif

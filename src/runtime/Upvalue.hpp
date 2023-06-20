#ifndef LISP_SRC_RUNTIME_UPVALUE_HPP_
#define LISP_SRC_RUNTIME_UPVALUE_HPP_

#include "../sexpr/SExpr.hpp"
#include <memory>
#include <vector>

namespace runtime {

class Upvalue {
  friend bool operator==(const Upvalue &lhs, const Upvalue &rhs);

private:
  const std::vector<const sexpr::SExpr *>::size_type stackPos;
  std::vector<const sexpr::SExpr *> &stack;

  const sexpr::SExpr *value;

  bool isOpen() const;

public:
  Upvalue(const std::vector<const sexpr::SExpr *>::size_type stackPos,
          std::vector<const sexpr::SExpr *> &stack);

  void close();

  const sexpr::SExpr *get() const;
  void set(const sexpr::SExpr *sexpr);
};

bool operator==(const Upvalue &lhs, const Upvalue &rhs);

} // namespace runtime

#endif

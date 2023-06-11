#ifndef LISP_SRC_VM_UPVALUE_HPP_
#define LISP_SRC_VM_UPVALUE_HPP_

#include "../common/sexpr/SExpr.hpp"
#include <memory>
#include <vector>

class Upvalue {
  friend bool operator==(const Upvalue &lhs, const Upvalue &rhs);

private:
  const std::vector<const SExpr *>::size_type stackPos;
  std::vector<const SExpr *> &stack;

  const SExpr *value;

  bool isOpen() const;

public:
  Upvalue(const std::vector<const SExpr *>::size_type stackPos,
          std::vector<const SExpr *> &stack);

  void close();

  const SExpr *get() const;
  void set(const SExpr *sexpr);
};

#endif

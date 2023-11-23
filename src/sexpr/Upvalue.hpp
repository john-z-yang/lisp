#ifndef LISP_SRC_SEXPR_UPVALUE_HPP_
#define LISP_SRC_SEXPR_UPVALUE_HPP_

#include "../runtime/StackPtr.hpp"
#include "Atom.hpp"
#include "SExpr.hpp"
#include <functional>
#include <memory>
#include <optional>
#include <vector>

namespace sexpr {

class Upvalue final : public Atom {
protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

private:
  runtime::StackPtr stackPos;
  std::reference_wrapper<std::vector<sexpr::SExpr *>> stack;

  std::optional<sexpr::SExpr *> ref;

  bool isOpen() const;

public:
  Upvalue(const runtime::StackPtr stackPos, std::vector<sexpr::SExpr *> &stack);

  void close();

  sexpr::SExpr *get() const;
  void set(sexpr::SExpr *sexpr);

  void fixupAddrs(const runtime::BreakTable &breakTable) override;

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif

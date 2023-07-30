#ifndef LISP_SRC_RUNTIME_ENV_HPP_
#define LISP_SRC_RUNTIME_ENV_HPP_

#include "../sexpr/SExpr.hpp"
#include "../sexpr/Sym.hpp"
#include <memory>
#include <unordered_map>
#include <unordered_set>

namespace runtime {

class Env {
  using SymTable =
      std::unordered_map<std::reference_wrapper<const sexpr::Sym>,
                         std::reference_wrapper<const sexpr::SExpr>,
                         sexpr::Sym::HashFunction, sexpr::Sym::EqualFunction>;

  using Macros =
      std::unordered_set<std::reference_wrapper<const sexpr::Sym>,
                         sexpr::Sym::HashFunction, sexpr::Sym::EqualFunction>;

private:
  SymTable symTable;
  Macros macros;

public:
  void def(const sexpr::Sym &sym, const sexpr::SExpr &val);
  void set(const sexpr::Sym &sym, const sexpr::SExpr &val);

  const SymTable &getSymTable() const;
  const sexpr::SExpr &find(const sexpr::Sym &sym);

  void regMacro(const sexpr::Sym &sym);
  bool isMacro(const sexpr::Sym &sym);
};

} // namespace runtime

#endif

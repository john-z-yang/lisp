#ifndef LISP_SRC_RUNTIME_ENV_HPP_
#define LISP_SRC_RUNTIME_ENV_HPP_

#include "../sexpr/SExpr.hpp"
#include "../sexpr/Sym.hpp"
#include <memory>
#include <unordered_map>
#include <unordered_set>

namespace runtime {

class Env {
  typedef std::unordered_map<const sexpr::Sym *, const sexpr::SExpr *,
                             sexpr::Sym::HashFunction,
                             sexpr::Sym::EqualFunction>
      SymVals;

  typedef std::unordered_set<const sexpr::Sym *, sexpr::Sym::HashFunction,
                             sexpr::Sym::EqualFunction>
      Macros;

private:
  SymVals symTable;
  Macros macros;

public:
  void def(const sexpr::Sym *sym, const sexpr::SExpr *val);

  void set(const sexpr::Sym *sym, const sexpr::SExpr *val);

  const SymVals &getSymTable() const;

  const sexpr::SExpr *find(const sexpr::Sym *sym);

  void defMacro(const sexpr::Sym *sym);

  bool isMacro(const sexpr::Sym *sym);
};

} // namespace runtime

#endif

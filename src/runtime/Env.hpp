#ifndef LISP_SRC_RUNTIME_ENV_HPP_
#define LISP_SRC_RUNTIME_ENV_HPP_

#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/Sym.hpp"
#include "BreakTable.hpp"
#include <functional>
#include <initializer_list>
#include <memory>
#include <unordered_map>
#include <unordered_set>

namespace runtime {

class Env {
  using SymSet = std::unordered_set<sexpr::Sym *>;

public:
  using SymTable = std::unordered_map<sexpr::Sym *, sexpr::SExpr *>;

private:
  SymTable symTable;
  SymSet macros;
  SymSet natFns;

  void regMacro(sexpr::Sym *sym);
  void regNative(sexpr::Sym *sym);
  void guardMutation(sexpr::Sym *sym);

public:
  void def(sexpr::Sym *sym, sexpr::SExpr *val);
  void defMacro(sexpr::Sym *sym, sexpr::SExpr *val);
  void defNatFn(sexpr::Sym *sym, sexpr::NatFn *natFn);

  void defNatFns(const std::initializer_list<
                 std::tuple<sexpr::Sym *, sexpr::NatFn *>> natFns);

  void set(sexpr::Sym *sym, sexpr::SExpr *val);
  sexpr::SExpr *load(sexpr::Sym *sym);

  bool isMacro(sexpr::Sym *sym);
  bool isNatFn(sexpr::Sym *sym);

  const SymTable &getSymTable() const;

  void fixupAddrs(const BreakTable &breakTable);
};

} // namespace runtime

#endif

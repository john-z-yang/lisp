#ifndef LISP_SRC_RUNTIME_ENV_HPP_
#define LISP_SRC_RUNTIME_ENV_HPP_

#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/Sym.hpp"
#include <functional>
#include <initializer_list>
#include <memory>
#include <unordered_map>
#include <unordered_set>

namespace runtime {

class Env {
private:
  std::unordered_map<const sexpr::Sym *, const sexpr::SExpr *> symTable;
  std::unordered_set<const sexpr::Sym *> macros;
  std::unordered_set<const sexpr::Sym *> builtInFns;

  void regMacro(const sexpr::Sym *sym);
  void regBuiltIn(const sexpr::Sym *sym);
  void guardMutation(const sexpr::Sym *sym);

public:
  void def(const sexpr::Sym *sym, const sexpr::SExpr *val);
  void defMacro(const sexpr::Sym *sym, const sexpr::SExpr *val);
  void defBuiltIn(const sexpr::Sym *sym, const sexpr::NatFn *natFn);

  void
  defBuiltInFns(const std::initializer_list<
                std::tuple<const sexpr::Sym *, const sexpr::NatFn *>> builtInFns
  );

  void set(const sexpr::Sym *sym, const sexpr::SExpr *val);
  const sexpr::SExpr *load(const sexpr::Sym *sym);

  bool isMacro(const sexpr::Sym *sym);
  bool isNatFn(const sexpr::Sym *sym);

  const std::unordered_map<const sexpr::Sym *, const sexpr::SExpr *> &
  getSymTable() const;
};

} // namespace runtime

#endif

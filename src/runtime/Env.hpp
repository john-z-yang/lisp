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
  using SymTable = std::unordered_map<
      std::reference_wrapper<const sexpr::Sym>,
      std::reference_wrapper<const sexpr::SExpr>,
      sexpr::Sym::HashFunction,
      sexpr::Sym::EqualFunction>;

  using SymSet = std::unordered_set<
      std::reference_wrapper<const sexpr::Sym>,
      sexpr::Sym::HashFunction,
      sexpr::Sym::EqualFunction>;

private:
  SymTable symTable;
  SymSet macros;
  SymSet natFns;

  void regMacro(const sexpr::Sym &sym);
  void regNative(const sexpr::Sym &sym);
  void guardMutation(const sexpr::Sym &sym);

public:
  void def(const sexpr::Sym &sym, const sexpr::SExpr &val);
  void defMacro(const sexpr::Sym &sym, const sexpr::SExpr &val);
  void defNatFn(const sexpr::Sym &sym, const sexpr::NatFn &natFn);

  void defNatFns(const std::initializer_list<
                 std::tuple<const sexpr::Sym &, const sexpr::NatFn &>> natFns);

  void set(const sexpr::Sym &sym, const sexpr::SExpr &val);
  const sexpr::SExpr &load(const sexpr::Sym &sym);

  bool isMacro(const sexpr::Sym &sym);
  bool isNatFn(const sexpr::Sym &sym);

  const SymTable &getSymTable() const;
};

} // namespace runtime

#endif

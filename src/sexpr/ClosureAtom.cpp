#include "ClosureAtom.hpp"
#include "../eval/EvalException.hpp"
#include "../eval/eval.hpp"
#include "NilAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <sstream>
#include <string>

ClosureAtom::ClosureAtom(Proc proc, const std::shared_ptr<Env> outerEnv,
                         const std::shared_ptr<SExpr> argNames)
    : ClosureAtom(proc, outerEnv, argNames, false) {}

ClosureAtom::ClosureAtom(Proc proc, const std::shared_ptr<Env> outerEnv,
                         const std::shared_ptr<SExpr> argNames,
                         const bool isMacro)
    : Atom(SExpr::Type::CLOSURE), proc(proc), outerEnv(outerEnv),
      argNames(argNames), isMacro(isMacro) {}

std::string ClosureAtom::toString() const {
  return isMacro ? "<macro>" : "<procedure>";
}

bool ClosureAtom::equals(const SExpr &other) const {
  if (isa<ClosureAtom>(other)) {
    return &proc == &dynamic_cast<const ClosureAtom &>(other).proc;
  }
  return false;
}

bool ClosureAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}

const std::string ClosureAtom::typeName = "<closure>";
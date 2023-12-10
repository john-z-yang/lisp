#include "CPPFnImpls.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/Bool.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/String.hpp"
#include "../sexpr/Sym.hpp"
#include <cmath>
#include <cstdlib>
#include <exception>
#include <memory>
#include <stdexcept>

using namespace sexpr;
using namespace fn;
using namespace runtime;

long genSymCnt = 0;
const SExpr *fn::genSym(StackIter, const uint8_t, VM &vm) {
  std::stringstream ss;
  ss << ";gensym-" << genSymCnt;
  genSymCnt += 1;
  return vm.heap.alloc<Sym>(ss.str());
}

const SExpr *fn::numAbs(StackIter params, const uint8_t, VM &vm) {
  return vm.heap.alloc<Num>(abs(cast<Num>(*params)->val));
}
const SExpr *fn::numMod(StackIter params, const uint8_t, VM &vm) {
  const auto lhs = cast<Num>(*params)->val;
  ++params;
  const auto rhs = cast<Num>(*params)->val;
  return vm.heap.alloc<Num>(std::fmod(lhs, rhs));
}

const SExpr *fn::strLen(StackIter params, const uint8_t, VM &vm) {
  return vm.heap.alloc<Num>(cast<String>(*params)->escaped.size());
}
const SExpr *fn::strApp(StackIter params, const uint8_t argc, VM &vm) {
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << cast<String>(*params)->escaped;
    ++params;
  }
  ss << "\"";
  return vm.heap.alloc<String>(ss.str());
}
const SExpr *fn::substr(StackIter params, const uint8_t, VM &vm) {
  const auto str = cast<String>(*params);
  const auto pos = cast<Num>(*(params + 1))->val;
  const auto end = cast<Num>(*(params + 2))->val;
  std::stringstream ss;
  try {
    ss << "\"" << str->escaped.substr(pos, end - pos) << "\"";
  } catch (std::out_of_range &ofr) {
    std::stringstream ess;
    ess << "Invalid range to substring for " << str->val << " (" << pos << ", "
        << end << ")";
    throw std::invalid_argument(ess.str());
  }
  return vm.heap.alloc<String>(ss.str());
}
const SExpr *fn::toStr(StackIter params, const uint8_t argc, VM &vm) {
  if (isa<String>(*params)) {
    return *params;
  }
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << **params;
    ++params;
  }
  ss << "\"";
  return vm.heap.alloc<String>(ss.str());
}

const SExpr *fn::cons(StackIter params, const uint8_t, VM &vm) {
  return vm.heap.alloc<SExprs>(*params, *(params + 1));
}
const SExpr *fn::car(StackIter params, const uint8_t, VM &) {
  return cast<SExprs>(*params)->first;
}
const SExpr *fn::cdr(StackIter params, const uint8_t, VM &) {
  return cast<SExprs>(*params)->rest;
}

const SExpr *fn::dis(StackIter params, const uint8_t, VM &vm) {
  cast<Closure>(*params)->dissassemble(std::cout);
  return vm.heap.alloc<Nil>();
}
const SExpr *fn::display(StackIter params, const uint8_t, VM &vm) {
  if (isa<String>(*params)) {
    const auto stringAtom = cast<String>(*params);
    std::cout << stringAtom->escaped;
  } else {
    std::cout << **params;
  }
  return vm.heap.alloc<Nil>();
}
const SExpr *fn::newline(StackIter, const uint8_t, VM &vm) {
  std::cout << std::endl;
  return vm.heap.alloc<Nil>();
}

const SExpr *fn::quit(StackIter, const uint8_t, VM &) {
  std::cout << "Farewell." << std::endl;
  exit(0);
}
const SExpr *fn::error(StackIter params, const uint8_t, VM &) {
  std::stringstream ss;
  ss << *params;
  throw std::runtime_error(ss.str());
}

const SExpr *fn::eq(StackIter params, const uint8_t, VM &vm) {
  const auto lhs = *params;
  const auto rhs = *(params + 1);
  if (isa<Sym>(lhs)) {
    return vm.heap.alloc<Bool>(*lhs == *rhs);
  }
  return vm.heap.alloc<Bool>(lhs == rhs);
}
const SExpr *fn::eqv(StackIter params, const uint8_t, VM &vm) {
  const auto lhs = *params;
  const auto rhs = *(params + 1);
  if (isa<Sym>(lhs) || isa<Num>(lhs)) {
    return vm.heap.alloc<Bool>(*lhs == *rhs);
  }
  return vm.heap.alloc<Bool>(lhs == rhs);
}
const SExpr *fn::equal(StackIter params, const uint8_t, VM &vm) {
  const auto lhs = *params;
  const auto rhs = *(params + 1);
  return vm.heap.alloc<Bool>(*lhs == *rhs);
}

const SExpr *fn::apply(StackIter params, const uint8_t argc, VM &vm) {
  const auto newArgs = *(params + argc - 1);

  vm.stack.erase(params + argc - 1);
  vm.stack.erase(params - 1);

  const auto newArgc = vm.unpackList(newArgs);

  vm.call(argc + newArgc - 2);

  return vm.heap.alloc<Nil>();
}

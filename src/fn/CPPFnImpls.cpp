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
const SExpr &fn::genSym(
    [[maybe_unused]] StackIter params,
    [[maybe_unused]] const uint8_t argc,
    VM &vm
) {
  std::stringstream ss;
  ss << ";gensym-" << genSymCnt;
  genSymCnt += 1;
  return vm.freeStore.alloc<Sym>(ss.str());
}

const SExpr &
fn::numAbs(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  return vm.freeStore.alloc<Num>(abs(cast<Num>(params->get()).val));
}
const SExpr &
fn::numMod(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  const auto lhs = cast<Num>(params->get()).val;
  ++params;
  const auto rhs = cast<Num>(params->get()).val;
  return vm.freeStore.alloc<Num>(std::fmod(lhs, rhs));
}

const SExpr &
fn::strLen(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  return vm.freeStore.alloc<Num>(cast<String>(params->get()).escaped.size());
}
const SExpr &fn::strApp(StackIter params, const uint8_t argc, VM &vm) {
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << cast<String>(params->get()).escaped;
    ++params;
  }
  ss << "\"";
  return vm.freeStore.alloc<String>(ss.str());
}
const SExpr &
fn::substr(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  const auto &str = cast<String>(params->get());
  const auto &pos = cast<Num>((params + 1)->get()).val;
  const auto &end = cast<Num>((params + 2)->get()).val;
  std::stringstream ss;
  try {
    ss << "\"" << str.escaped.substr(pos, end - pos) << "\"";
  } catch (std::out_of_range &ofr) {
    std::stringstream ess;
    ess << "Invalid range to substring for " << str.val << " (" << pos << ", "
        << end << ")";
    throw std::invalid_argument(ess.str());
  }
  return vm.freeStore.alloc<String>(ss.str());
}
const SExpr &fn::toStr(StackIter params, const uint8_t argc, VM &vm) {
  if (isa<String>(params->get())) {
    return params->get();
  }
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << params->get();
    ++params;
  }
  ss << "\"";
  return vm.freeStore.alloc<String>(ss.str());
}

const SExpr &
fn::cons(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  return vm.freeStore.alloc<SExprs>(params->get(), *(params + 1));
}
const SExpr &fn::car(
    StackIter params,
    [[maybe_unused]] const uint8_t argc,
    [[maybe_unused]] VM &vm
) {
  return cast<SExprs>(params->get()).first;
}
const SExpr &fn::cdr(
    StackIter params,
    [[maybe_unused]] const uint8_t argc,
    [[maybe_unused]] VM &vm
) {
  return cast<SExprs>(params->get()).rest;
}

const SExpr &
fn::dis(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  cast<Closure>(params->get()).dissassemble(std::cout);
  return vm.freeStore.alloc<Nil>();
}
const SExpr &
fn::display(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  if (isa<String>(params->get())) {
    const auto &stringAtom = cast<String>(params->get());
    std::cout << stringAtom.escaped;
  } else {
    std::cout << params->get();
  }
  return vm.freeStore.alloc<Nil>();
}
const SExpr &fn::newline(
    [[maybe_unused]] StackIter params,
    [[maybe_unused]] const uint8_t argc,
    VM &vm
) {
  std::cout << std::endl;
  return vm.freeStore.alloc<Nil>();
}

const SExpr &fn::quit(
    [[maybe_unused]] StackIter params,
    [[maybe_unused]] const uint8_t argc,
    [[maybe_unused]] VM &vm
) {
  std::cout << "Farewell." << std::endl;
  exit(0);
}
const SExpr &fn::error(
    StackIter params,
    [[maybe_unused]] const uint8_t argc,
    [[maybe_unused]] VM &vm
) {
  std::stringstream ss;
  ss << params->get();
  throw std::runtime_error(ss.str());
}

const SExpr &
fn::eq(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  const auto &lhs = params->get();
  const auto &rhs = (params + 1)->get();
  if (isa<Sym>(lhs)) {
    return vm.freeStore.alloc<Bool>(lhs == rhs);
  }
  return vm.freeStore.alloc<Bool>(&lhs == &rhs);
}
const SExpr &
fn::eqv(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  const auto &lhs = params->get();
  const auto &rhs = (params + 1)->get();
  if (isa<Sym>(lhs) || isa<Num>(lhs)) {
    return vm.freeStore.alloc<Bool>(lhs == rhs);
  }
  return vm.freeStore.alloc<Bool>(&lhs == &rhs);
}
const SExpr &
fn::equal(StackIter params, [[maybe_unused]] const uint8_t argc, VM &vm) {
  const auto &lhs = params->get();
  const auto &rhs = (params + 1)->get();
  return vm.freeStore.alloc<Bool>(lhs == rhs);
}

const SExpr &fn::apply(StackIter params, const uint8_t argc, VM &vm) {
  const auto &newArgs = (params + argc - 1)->get();

  vm.stack.erase(params + argc - 1);
  vm.stack.erase(params - 1);

  const auto newArgc = vm.unpackList(newArgs);

  vm.call(argc + newArgc - 2);

  return vm.freeStore.alloc<Nil>();
}

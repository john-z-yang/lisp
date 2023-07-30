#ifndef LISP_SRC_FN_CPPFNIMPLS_HPP_
#define LISP_SRC_FN_CPPFNIMPLS_HPP_

#include "../runtime/VM.hpp"
#include "../sexpr/Bool.hpp"
#include "CPPFn.hpp"
#include <cstddef>
#include <functional>
#include <memory>
#include <vector>

namespace fn {

template <typename T, template <typename> typename Op>
const sexpr::SExpr &compare(runtime::StackIter params, const uint8_t argc,
                            runtime::VM &vm) {
  Op<typename T::ValueType> op;
  typename T::ValueType prev = cast<T>(params->get()).val;
  ++params;
  for (uint8_t i{1}; i < argc; ++i) {
    typename T::ValueType cur = cast<T>(params->get()).val;
    if (!op(prev, cur)) {
      return vm.freeStore.alloc<sexpr::Bool>(false);
    }
    prev = cur;
    ++params;
  }
  return vm.freeStore.alloc<sexpr::Bool>(true);
}

template <typename T, template <typename> typename Op, auto init>
const sexpr::SExpr &accum(runtime::StackIter params, const uint8_t argc,
                          runtime::VM &vm) {
  Op<typename T::ValueType> op;
  typename T::ValueType acc = init;
  for (uint8_t i{0}; i < argc; ++i) {
    acc = op(acc, cast<T>(params->get()).val);
    ++params;
  }
  return vm.freeStore.alloc<T>(acc);
}

template <typename T, template <typename> typename BinOp,
          template <typename> typename UniOp>
const sexpr::SExpr &dimi(runtime::StackIter params, const uint8_t argc,
                         runtime::VM &vm) {
  UniOp<typename T::ValueType> uniOp;
  typename T::ValueType acc = cast<T>(params->get()).val;
  if (argc == 1) {
    return vm.freeStore.alloc<T>(uniOp(acc));
  }
  ++params;
  BinOp<typename T::ValueType> biOp;
  for (uint8_t i{1}; i < argc; ++i) {
    acc = biOp(acc, cast<T>(params->get()).val);
    ++params;
  }
  return vm.freeStore.alloc<T>(acc);
}

template <typename... T>
const sexpr::SExpr &typePred(runtime::StackIter params,
                             [[maybe_unused]] const uint8_t argc,
                             runtime::VM &vm) {
  return vm.freeStore.alloc<sexpr::Bool>((isa<T>(params->get()) || ...));
}

template <class T> struct inverse {
  constexpr auto operator()(const T &x) const { return 1.0 / x; }
};

CPPFn genSym;

CPPFn numAbs;
CPPFn numMod;

CPPFn strLen;
CPPFn strApp;
CPPFn substr;
CPPFn toStr;

CPPFn cons;
CPPFn car;
CPPFn cdr;

CPPFn dis;
CPPFn display;
CPPFn newline;

CPPFn quit;
CPPFn error;

CPPFn eq;
CPPFn eqv;
CPPFn equal;

CPPFn apply;

} // namespace fn

#endif

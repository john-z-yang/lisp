#ifndef LISP_SRC_RUNTIME_NATFNIMPLS_HPP_
#define LISP_SRC_RUNTIME_NATFNIMPLS_HPP_

#include "../sexpr/Bool.hpp"
#include "CPPFn.hpp"
#include "VM.hpp"
#include <cstddef>
#include <functional>
#include <memory>
#include <vector>

namespace runtime {

template <typename T, template <typename> typename Op>
const sexpr::SExpr &lispCmpOp(StackIter params, const uint8_t argc, VM &vm) {
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
const sexpr::SExpr &lispAcum(StackIter params, const uint8_t argc, VM &vm) {
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
const sexpr::SExpr &lispDim(StackIter params, const uint8_t argc, VM &vm) {
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
const sexpr::SExpr &lispTypePred(StackIter params,
                                 [[maybe_unused]] const uint8_t argc, VM &vm) {
  return vm.freeStore.alloc<sexpr::Bool>((isa<T>(params->get()) || ...));
}

template <class T> struct inverse {
  constexpr auto operator()(const T &x) const { return 1.0 / x; }
};

CPPFn lispGenSym;

CPPFn lispNumEq;
CPPFn lispGt;
CPPFn lispGteq;
CPPFn lispLt;
CPPFn lispLteq;
CPPFn lispAbs;
CPPFn lispMod;

CPPFn lispStrLen;
CPPFn lispStrSub;
CPPFn lispStrCon;
CPPFn lispToStr;

CPPFn lispCons;
CPPFn lispCar;
CPPFn lispCdr;

CPPFn lispDis;
CPPFn lispDisplay;
CPPFn lispNewline;

CPPFn lispQuit;
CPPFn lispError;

CPPFn lispEq;
CPPFn lispEqv;
CPPFn lispEqual;

CPPFn lispApply;

} // namespace runtime

#endif

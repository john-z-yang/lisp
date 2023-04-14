#include "Env.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/NativeFunctionAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "../vm/RuntimeException.hpp"
#include <iostream>
#include <memory>

Env::Env() {
  symTable.insert({SymAtom("display"),
                   std::make_shared<NativeFunctionAtom>(&lispDisplay, 1)});
  symTable.insert(
      {SymAtom("="), std::make_shared<NativeFunctionAtom>(&lispEq, -1)});
  symTable.insert(
      {SymAtom("+"), std::make_shared<NativeFunctionAtom>(&lispAdd, -1)});
  symTable.insert(
      {SymAtom("-"), std::make_shared<NativeFunctionAtom>(&lispSub, -1)});
  symTable.insert(
      {SymAtom("*"), std::make_shared<NativeFunctionAtom>(&lispMult, -1)});
  symTable.insert(
      {SymAtom("/"), std::make_shared<NativeFunctionAtom>(&lispDiv, -1)});
  symTable.insert(
      {SymAtom("%"), std::make_shared<NativeFunctionAtom>(&lispMod, 2)});
}

void Env::def(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is already defined.");
  }
  symTable.insert({sym, val});
}

void Env::set(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is not defined.");
  }
  symTable[sym] = val;
}

std::shared_ptr<SExpr> Env::find(SymAtom &sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is not defined.");
  }
  return it->second;
}

std::shared_ptr<SExpr>
lispDisplay(std::vector<std::shared_ptr<SExpr>>::iterator params,
            const uint8_t argc) {
  std::cout << **params << std::endl;
  return std::make_shared<NilAtom>();
}

std::shared_ptr<BoolAtom>
lispEq(std::vector<std::shared_ptr<SExpr>>::iterator params,
       const uint8_t argc) {
  const auto val = cast<IntAtom>(*params)->val;
  for (auto i = 0; i < argc; i += 1) {
    if (cast<IntAtom>(*params)->val != val) {
      return std::make_shared<BoolAtom>(false);
    }
    ++params;
  }
  return std::make_shared<BoolAtom>(true);
}

std::shared_ptr<IntAtom>
lispAdd(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  int res = 0;
  for (auto i = 0; i < argc; i += 1) {
    res += cast<IntAtom>(*params)->val;
    ++params;
  }
  return std::make_shared<IntAtom>(res);
}

std::shared_ptr<IntAtom>
lispSub(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  if (argc == 1) {
    return std::make_shared<IntAtom>(-cast<IntAtom>(*params)->val);
  }
  int res = cast<IntAtom>(*params)->val;
  ++params;
  for (auto i = 1; i < argc; i += 1) {
    res -= cast<IntAtom>(*params)->val;
    ++params;
  }
  return std::make_shared<IntAtom>(res);
}

std::shared_ptr<IntAtom>
lispMult(std::vector<std::shared_ptr<SExpr>>::iterator params,
         const uint8_t argc) {
  int res = 1;
  for (auto i = 0; i < argc; i += 1) {
    res *= cast<IntAtom>(*params)->val;
    ++params;
  }
  return std::make_shared<IntAtom>(res);
}

std::shared_ptr<IntAtom>
lispDiv(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  if (argc == 1) {
    return std::make_shared<IntAtom>(1 / cast<IntAtom>(*params)->val);
  }
  int res = cast<IntAtom>(*params)->val;
  ++params;
  for (auto i = 1; i < argc; i += 1) {
    res /= cast<IntAtom>(*params)->val;
    ++params;
  }
  return std::make_shared<IntAtom>(res);
}

std::shared_ptr<IntAtom>
lispMod(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  const auto lhs = cast<IntAtom>(*params)->val;
  ++params;
  const auto rhs = cast<IntAtom>(*params)->val;
  return std::make_shared<IntAtom>(lhs % rhs);
}
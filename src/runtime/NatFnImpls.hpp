#ifndef LISP_SRC_VM_NATIVEFNS_HPP_
#define LISP_SRC_VM_NATIVEFNS_HPP_

#include "../common/sexpr/SExpr.hpp"
#include <memory>
#include <vector>

typedef std::shared_ptr<SExpr>(NativeFn)(
    std::vector<std::shared_ptr<SExpr>>::iterator params, const uint8_t argc);

NativeFn lispIsSym;

NativeFn lispIsNum;
NativeFn lispEq;
NativeFn lispGt;
NativeFn lispGteq;
NativeFn lispLt;
NativeFn lispLteq;
NativeFn lispAdd;
NativeFn lispSub;
NativeFn lispMult;
NativeFn lispDiv;
NativeFn lispAbs;
NativeFn lispMod;

NativeFn lispIsStr;
NativeFn lispStrLen;
NativeFn lispStrSub;
NativeFn lispStrCon;
NativeFn lispToStr;

NativeFn lispIsNull;
NativeFn lispIsCons;
NativeFn lispCons;
NativeFn lispCar;
NativeFn lispCdr;

NativeFn lispDis;
NativeFn lispIsClosure;
NativeFn lispDisplay;
NativeFn lispQuit;

#endif

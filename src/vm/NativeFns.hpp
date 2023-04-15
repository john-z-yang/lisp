#ifndef LISP_SRC_VM_NATIVEFNS_HPP_
#define LISP_SRC_VM_NATIVEFNS_HPP_

#include "../sexpr/SExpr.hpp"
#include <memory>
#include <vector>

typedef std::shared_ptr<SExpr>(NativeFn)(
    std::vector<std::shared_ptr<SExpr>>::iterator params, const uint8_t argc);

NativeFn lispDisplay;

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

#endif

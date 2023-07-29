#ifndef LISP_SRC_RUNTIME_NATFNIMPLS_HPP_
#define LISP_SRC_RUNTIME_NATFNIMPLS_HPP_

#include "CPPFn.hpp"
#include <memory>
#include <vector>

namespace runtime {

CPPFn lispIsSym;
CPPFn lispGenSym;

CPPFn lispIsNum;
CPPFn lispNumEq;
CPPFn lispGt;
CPPFn lispGteq;
CPPFn lispLt;
CPPFn lispLteq;
CPPFn lispAdd;
CPPFn lispSub;
CPPFn lispMult;
CPPFn lispDiv;
CPPFn lispAbs;
CPPFn lispMod;

CPPFn lispIsStr;
CPPFn lispStrLen;
CPPFn lispStrSub;
CPPFn lispStrCon;
CPPFn lispToStr;

CPPFn lispIsNull;
CPPFn lispIsCons;
CPPFn lispCons;
CPPFn lispCar;
CPPFn lispCdr;

CPPFn lispDis;
CPPFn lispIsClosure;
CPPFn lispDisplay;
CPPFn lispNewline;

CPPFn lispQuit;
CPPFn lispError;

CPPFn lispEq;
CPPFn lispEqv;
CPPFn lispEqual;

CPPFn lispIsProc;

CPPFn lispApply;

} // namespace runtime

#endif

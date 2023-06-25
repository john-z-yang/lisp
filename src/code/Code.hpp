#ifndef LISP_SRC_CODE_CODE_HPP_
#define LISP_SRC_CODE_CODE_HPP_

#include "../sexpr/SExpr.hpp"
#include "InstrPtr.hpp"
#include <cstdint>
#include <iostream>
#include <memory>
#include <vector>

namespace code {
class Code {
  friend std::ostream &operator<<(std::ostream &o, const sexpr::SExpr &sExpr);

public:
  std::vector<uint8_t> byteCodes;
  std::vector<std::reference_wrapper<const sexpr::SExpr>> consts;

  std::vector<unsigned int> lineNums;

  uint8_t pushCode(const uint8_t code);
  uint8_t pushCode(const uint8_t code, const unsigned int lineNum);
  uint8_t pushConst(const sexpr::SExpr &sExpr);

  void patchJump(const InstrPtr idx);
};

std::ostream &operator<<(std::ostream &o, const Code &code);

} // namespace code

#endif

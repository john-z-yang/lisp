#ifndef LISP_SRC_CODE_CODE_HPP_
#define LISP_SRC_CODE_CODE_HPP_

#include "../sexpr/SExpr.hpp"
#include "../sexpr/SymAtom.hpp"
#include <cstdint>
#include <iostream>
#include <memory>
#include <vector>

class Code {
  friend std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);

public:
  std::vector<uint8_t> byteCodes;
  std::vector<std::shared_ptr<SExpr>> consts;

  std::vector<int> lineNums;

  uint8_t pushCode(const uint8_t code, const uint lineNum);
  uint8_t pushConst(std::shared_ptr<SExpr> sExpr);
  void patchJump(const std::vector<uint8_t>::size_type idx);
};

std::ostream &operator<<(std::ostream &o, const Code &code);
#endif
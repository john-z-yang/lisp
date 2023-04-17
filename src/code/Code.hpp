#ifndef LISP_SRC_CODE_CODE_HPP_
#define LISP_SRC_CODE_CODE_HPP_

#include "../sexpr/SExpr.hpp"
#include <cstdint>
#include <iostream>
#include <memory>
#include <vector>

class Code {
public:
  std::vector<uint8_t> byteCodes;
  std::vector<std::shared_ptr<SExpr>> consts;

  std::vector<unsigned int> lineNums;

  uint8_t pushCode(const uint8_t code);
  uint8_t pushCode(const uint8_t code, const unsigned int lineNum);
  uint8_t pushConst(std::shared_ptr<SExpr> sExpr);
  void patchJump(const std::vector<uint8_t>::size_type idx);

private:
  friend std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
};

std::ostream &operator<<(std::ostream &o, const Code &code);
#endif

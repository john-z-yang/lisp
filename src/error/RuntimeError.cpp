#include "RuntimeError.hpp"
#include <iomanip>

using namespace sexpr;
using namespace runtime;
using namespace error;

RuntimeError::RuntimeError(
    const std::string &msg,
    Env globals,
    std::vector<const sexpr::SExpr *> stack,
    std::vector<CallFrame> frames
)
    : _msg(msg), globals(globals), stack(stack), frames(frames) {}

const char *RuntimeError::what() const noexcept { return _msg.c_str(); }

std::ostream &error::operator<<(std::ostream &o, const RuntimeError &re) {
  std::unordered_map<const SExpr *, const Sym *> sExprSyms;
  for (const auto &p : re.globals.getSymTable()) {
    sExprSyms.insert({p.second, p.first});
  }

  const unsigned int PADDING_WIDTH = 4;
  const unsigned int IDX_WIDTH = 8;

  o << "In code object with ip: " << re.frames.back().ip << std::endl;

  o << std::setw(PADDING_WIDTH) << std::right
    << re.frames.back().closure->proto->code << "Call stack:";
  for (unsigned int idx = 0; const auto &stackFrame : re.frames) {
    o << std::endl
      << std::setw(PADDING_WIDTH) << "" << std::setw(IDX_WIDTH) << std::left
      << idx << "<Closure: " << stackFrame.closure << ", ip: " << stackFrame.ip
      << ", bp: " << stackFrame.bp << ">";
    idx += 1;
    auto it = sExprSyms.find(stackFrame.closure);
    if (it != sExprSyms.end()) {
      o << " (" << it->second << ")";
    }
  }

  o << std::endl << "Data stack:";
  for (unsigned int idx = 0; const auto &sexpr : re.stack) {
    o << std::endl
      << std::setw(PADDING_WIDTH) << "" << std::setw(IDX_WIDTH) << std::left
      << idx << *sexpr;
    idx += 1;
    auto it = sExprSyms.find(sexpr);
    if (it != sExprSyms.end()) {
      o << " (" << it->second << ")";
    }
  }
  return o << std::endl << re.what();
}

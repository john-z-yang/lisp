#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../code/Code.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/Fn.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/Sym.hpp"
#include "Local.hpp"
#include "Token.hpp"
#include "Upvalue.hpp"
#include <functional>
#include <memory>
#include <optional>
#include <unordered_map>
#include <vector>

namespace compile {

class Compiler {
private:
  using SrcMap = std::unordered_map<const sexpr::SExpr *, SrcLoc>;
  using TokenIter = std::vector<Token>::const_iterator;
  using Visitor = std::function<void(const sexpr::SExpr &)>;

  runtime::VM &vm;
  const std::optional<std::reference_wrapper<Compiler>> enclosing;

  std::vector<std::string> source;
  SrcMap srcMap;
  unsigned int curLine;

  const sexpr::SExpr &argNames;
  const uint8_t arity;
  const bool variadic;

  const sexpr::SExprs &body;

  std::vector<Local> locals;
  std::vector<Upvalue> upValues;
  uint8_t stackOffset;

  code::Code code;

  static bool isNum(const std::string s);
  static std::vector<Token> tokenize(std::vector<std::string> lines);
  static std::vector<Token> tokenize(std::string line, const unsigned int row);
  static void handleUnexpectedToken(const Token &token,
                                    const std::string &line);

  const sexpr::SExprs &parse();
  const sexpr::SExpr &parseLists(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr &parseList(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr &parseElem(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr &parseSexprs(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr &parseAtom(Token token);

  Compiler(const std::vector<std::string> source, SrcMap sourceLoc,
           const sexpr::SExpr &param, const sexpr::SExprs &body,
           Compiler &enclosing, runtime::VM &vm);

  void updateCurLine(const sexpr::SExpr &sExpr);
  std::optional<const std::size_t> resolveLocal(const sexpr::Sym &sym);
  std::optional<const std::size_t> resolveUpvalue(Compiler &caller,
                                                  const sexpr::Sym &sym);
  std::size_t addUpvalue(int idx, bool isLocal);
  bool isVariadic();
  uint8_t countArity();

  template <typename T> code::InstrPtr emitCode(T v) {
    return code.pushCode(v, curLine);
  }
  template <typename T, typename... Args>
  code::InstrPtr emitCode(T first, Args... args) {
    const auto idx = emitCode(first);
    emitCode(args...);
    return idx;
  }
  code::InstrPtr emitConst(const sexpr::SExpr &sExpr);
  void patchJump(const code::InstrPtr idx);

  const sexpr::SExpr &at(const unsigned int n, const sexpr::SExpr &sExpr);
  const sexpr::SExpr &last(const sexpr::SExpr &sExpr);
  unsigned int visitEach(const sexpr::SExpr &sExpr, Visitor visitor);
  void traverse(const sexpr::SExpr &sExpr, Visitor visitor);
  void compileStmt(const sexpr::SExpr &sExpr);
  void compileExpr(const sexpr::SExpr &sExpr);
  void compileLambda(const sexpr::SExpr &sExpr);
  void compileCall(const sexpr::SExprs &sExprs);
  void compileAtom(const sexpr::Atom &atom);
  void compileSym(const sexpr::Sym &sym);
  void compileQuote(const sexpr::SExpr &sExpr);
  void compileDef(const sexpr::SExpr &sExpr);
  void compileSet(const sexpr::SExpr &sExpr);
  void compileIf(const sexpr::SExpr &sExpr);
  void compileRet();
  void execDefMacro(const sexpr::SExpr &sExpr);
  const sexpr::SExpr &execMacro(const sexpr::SExpr &macro);

  void handleSyntaxError(const std::string grammar, const std::string expected,
                         const sexpr::SExpr &actual);

public:
  Compiler(std::vector<std::string> source, runtime::VM &vm);

  const sexpr::Fn &compile();

  static void verifyLex(const std::string &line, const unsigned int lineNum,
                        unsigned int &openParen, unsigned int &closedParen);
};

} // namespace compile

#endif

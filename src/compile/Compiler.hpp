#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../common/Code.hpp"
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
#include <unordered_map>
#include <vector>

namespace compile {

class Compiler {
private:
  typedef std::unordered_map<const sexpr::SExpr *,
                             std::tuple<const unsigned int, const unsigned int>>
      SourceLoc;

  typedef std::function<void(const sexpr::SExpr *)> Visitor;

  runtime::VM &vm;

  Compiler *const enclosing;

  std::vector<std::string> source;
  SourceLoc sourceLoc;

  const sexpr::SExpr *const params;
  const sexpr::SExprs *const body;

  std::vector<Local> locals;
  std::vector<Upvalue> upValues;
  uint8_t stackOffset;

  Code code;

  // Parse
  static std::vector<Token> tokenize(std::vector<std::string> lines);
  static std::vector<Token> tokenize(std::string line, const unsigned int row);
  static bool isNum(const std::string s);
  const sexpr::SExprs *parse();
  const sexpr::SExpr *parse(std::vector<Token>::const_iterator &it,
                            const std::vector<Token>::const_iterator &end);
  const sexpr::SExpr *
  parseSexprs(std::vector<Token>::const_iterator &it,
              const std::vector<Token>::const_iterator &end);
  const sexpr::SExpr *parseList(std::vector<Token>::const_iterator &it,
                                const std::vector<Token>::const_iterator &end);
  const sexpr::SExpr *parseAtom(Token token);
  static void handleUnexpectedToken(const Token &token,
                                    const std::string &line);

  // Sema
  Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
           const sexpr::SExpr *param, const sexpr::SExprs *body,
           Compiler *enclosing, runtime::VM &vm);
  int resolveLocal(const sexpr::Sym *sym);
  int resolveUpvalue(Compiler &caller, const sexpr::Sym *sym);
  int addUpvalue(int idx, bool isLocal);
  bool isVariadic();
  int countParams();

  // Codegen
  unsigned int visitEach(const sexpr::SExpr *sExpr, Visitor visitor);
  const sexpr::SExpr *at(const unsigned int n, const sexpr::SExpr *sExpr);
  void compileStmt(const sexpr::SExpr *sExpr);
  void compileExpr(const sexpr::SExpr *sExpr);
  void compileLambda(const sexpr::SExpr *sExpr);
  void compileCall(const sexpr::SExprs *sExprs);
  void compileAtom(const sexpr::Atom *atom);
  void compileSym(const sexpr::Sym *sym);
  void compileQuote(const sexpr::SExpr *sExpr);
  void compileDef(const sexpr::SExpr *sExpr);
  void compileDefMacro(const sexpr::SExpr *sExpr);
  void compileSet(const sexpr::SExpr *sExpr);
  void compileIf(const sexpr::SExpr *sExpr);
  void compileRet();
  const sexpr::SExpr *expandMacro(const sexpr::SExpr *macro);
  void handleSyntaxError(const std::string grammar, const std::string expected,
                         const sexpr::SExpr *const actual);

public:
  Compiler(std::vector<std::string> source, runtime::VM &vm);

  static void verifyLex(std::string &line, const unsigned int lineNum,
                        uint32_t &openParen, uint32_t &closedParen);

  const sexpr::Fn *compile();
};

} // namespace compile

#endif

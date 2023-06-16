#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../common/Code.hpp"
#include "../common/sexpr/FnAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include "../runtime/VM.hpp"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

class Compiler {
private:
  typedef std::unordered_map<const SExpr *,
                             std::tuple<const unsigned int, const unsigned int>>
      SourceLoc;

  struct Token {
    unsigned int row;
    unsigned int col;
    std::string str;
  };

  typedef std::function<void(const SExpr *)> Visitor;

  struct Local {
    const SymAtom *const symbol;
    const uint8_t stackOffset;
    bool isCaptured;
  };

  struct UpValue {
    int idx;
    bool isLocal;
  };

  std::vector<std::string> source;
  SourceLoc sourceLoc;

  VM &vm;

  Compiler *const enclosing;

  Code code;

  const SExpr *const arg;
  const SExpr *const body;

  std::vector<Local> locals;
  std::vector<UpValue> upValues;
  uint8_t stackOffset;

  Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
           const SExpr *arg, const SExpr *body, Compiler *enclosing, VM &vm);

  static std::vector<Token> tokenize(std::vector<std::string> lines);
  static std::vector<Token> tokenize(std::string line, const unsigned int row);
  static bool isNum(const std::string s);

  const SExpr *parse();
  const SExpr *parse(std::vector<Token>::const_iterator &it,
                     const std::vector<Token>::const_iterator &end);
  const SExpr *parseSexprs(std::vector<Token>::const_iterator &it,
                           const std::vector<Token>::const_iterator &end);
  const SExpr *parseList(std::vector<Token>::const_iterator &it,
                         const std::vector<Token>::const_iterator &end);
  const SExpr *parseAtom(Token token);

  void compile(const SExpr *sExpr);
  void compileSym(const SymAtom *sym);
  void compileQuote(const SExpr *sExpr);
  void compileDef(const SExpr *sExpr);
  void compileDefMacro(const SExpr *sExpr);
  void compileSet(const SExpr *sExpr);
  void compileIf(const SExpr *sExpr);
  void compileLambda(const SExpr *sExpr);
  void compileCall(const SExprs *sExprs);

  const SExpr *expandMacro(const SExpr *macro);

  unsigned int visitEach(const SExpr *sExpr, Visitor visitor);
  const SExpr *at(const unsigned int n, const SExpr *sExpr);

  int resolveLocal(const SymAtom *sym);
  int resolveUpvalue(Compiler &caller, const SymAtom *sym);
  int addUpvalue(int idx, bool isLocal);

  static void handleUnexpectedToken(const Token &token,
                                    const std::string &line);
  void handleSyntaxError(const std::string grammar, const std::string expected,
                         const SExpr *const actual);

public:
  Compiler(std::vector<std::string> source, VM &vm);

  const FnAtom *compile();

  static void verifyLex(std::string &line, const unsigned int lineNum,
                        uint32_t &openParen, uint32_t &closedParen);
};

#endif

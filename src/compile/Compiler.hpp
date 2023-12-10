#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../code/Code.hpp"
#include "../runtime/GCGuard.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/Casting.hpp"
#include "../sexpr/Prototype.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/Sym.hpp"
#include "Local.hpp"
#include "Token.hpp"
#include "Upvalue.hpp"
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <tuple>
#include <unordered_map>
#include <vector>

namespace compile {

const sexpr::Sym BEGIN_SYM("begin");
const sexpr::Sym QUOTE_SYM("quote");
const sexpr::Sym SET_SYM("set!");
const sexpr::Sym IF_SYM("if");
const sexpr::Sym LAMBDA_SYM("lambda");
const sexpr::Sym DEFINE_SYM("define");
const sexpr::Sym DEFMACRO_SYM("defmacro");

class Compiler {
private:
  using SrcMap = std::unordered_map<const sexpr::SExpr *, SrcLoc>;
  using TokenIter = std::vector<Token>::const_iterator;
  using Visitor = std::function<void(const sexpr::SExpr *)>;

  runtime::VM &vm;
  const std::optional<runtime::GCGuard> gcGuard;
  const std::optional<std::reference_wrapper<Compiler>> enclosing;

  std::vector<std::string> source;
  SrcMap srcMap;
  SrcLoc curSrcLoc;

  const sexpr::SExpr *param;
  const uint8_t arity;
  const bool variadic;

  const sexpr::SExprs *body;

  std::vector<Local> locals;
  std::vector<Upvalue> upValues;
  uint8_t stackOffset;

  code::Code code;

  static bool isNum(const std::string s);
  static std::vector<Token> tokenize(std::vector<std::string> lines);
  static std::vector<Token> tokenize(std::string line, const unsigned int row);
  static void
  handleUnexpectedToken(const Token &token, const std::string &line);

  const sexpr::SExprs *parse();
  const sexpr::SExpr *parseLists(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseList(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseElem(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseSexprs(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseAtom(Token token);

  template <typename T> class MatchedSExpr {
  private:
    const T *sExpr;
    const std::function<void()> callBack;

  public:
    MatchedSExpr(const T *sExpr, const std::function<void()> callBack)
        : sExpr(sExpr), callBack(callBack) {}
    const T *get() const {
      callBack();
      return sExpr;
    }
  };

  template <typename First, typename... Rest>
  std::tuple<const MatchedSExpr<First>, const MatchedSExpr<Rest>...>
  unpack(const sexpr::SExpr *sExpr) {
    const auto sExprs = sexpr::cast<sexpr::SExprs>(sExpr);
    updateCurSrcLoc(sExprs);
    if constexpr (sizeof...(Rest) != 0) {
      return std::tuple_cat(
          std::tuple<const MatchedSExpr<First>>(MatchedSExpr<First>(
              cast<First>(sExprs->first),
              [this, sExprs]() { updateCurSrcLoc(sExprs); }
          )),
          unpack<Rest...>(sExprs->rest)
      );
    } else {
      assertType<sexpr::Nil>(sExprs->rest);
      return std::tuple<const MatchedSExpr<First>>(MatchedSExpr<First>(
          cast<First>(sExprs->first),
          [this, sExprs]() { updateCurSrcLoc(sExprs); }
      ));
    }
  }

  template <typename First, typename... Rest>
  std::tuple<
      const MatchedSExpr<First>,
      const MatchedSExpr<Rest>...,
      const MatchedSExpr<sexpr::SExpr>>
  unpackPartial(const sexpr::SExpr *sExpr) {
    const auto sExprs = sexpr::cast<sexpr::SExprs>(sExpr);
    updateCurSrcLoc(sExprs);
    if constexpr (sizeof...(Rest) != 0) {
      return std::tuple_cat(
          std::tuple<const MatchedSExpr<First>>(MatchedSExpr<First>(
              cast<First>(sExprs->first),
              [this, sExprs]() { updateCurSrcLoc(sExprs); }
          )),
          unpackPartial<Rest...>(sExprs->rest)
      );
    } else {
      return std::tuple<const MatchedSExpr<First>, MatchedSExpr<sexpr::SExpr>>(
          MatchedSExpr<First>(
              cast<First>(sExprs->first),
              [this, sExprs]() { updateCurSrcLoc(sExprs); }
          ),
          MatchedSExpr<sexpr::SExpr>(
              sExprs->rest, [this, sExprs]() { updateCurSrcLoc(sExprs); }
          )
      );
    }
  }

  template <typename First, typename... Rest>
  bool check(const sexpr::SExpr *sExpr) {
    if (!isa<sexpr::SExprs>(sExpr)) {
      return false;
    }
    const auto sExprs = cast<sexpr::SExprs>(sExpr);
    if constexpr (sizeof...(Rest) != 0) {
      return isa<First>(sExprs->first) && check<Rest...>(sExprs->rest);
    } else {
      return isa<First>(sExprs->first);
    }
  }

  bool matchForm(
      const sexpr::SExpr *sExpr,
      std::initializer_list<std::tuple<
          const sexpr::Sym *,
          const std::function<void(MatchedSExpr<sexpr::SExpr>)>>> rules,
      std::optional<const std::function<
          void(MatchedSExpr<sexpr::Sym>, MatchedSExpr<sexpr::SExpr>)>>
          wildCardHandler = std::nullopt
  ) {
    if (!check<sexpr::Sym>(sExpr)) {
      return false;
    }
    const auto [sym, rest] = unpackPartial<sexpr::Sym>(sExpr);
    for (const auto &[name, handler] : rules) {
      if (*name == *sym.get()) {
        handler(rest);
        return true;
      }
    }
    if (wildCardHandler) {
      wildCardHandler->operator()(sym, rest);
      return true;
    }
    return false;
  }

  Compiler(
      const std::vector<std::string> source,
      SrcMap sourceLoc,
      const sexpr::SExpr *param,
      const sexpr::SExprs *body,
      Compiler &enclosing,
      runtime::VM &vm
  );

  void updateCurSrcLoc(const sexpr::SExprs *sExpr);
  std::optional<const std::size_t> resolveLocal(const sexpr::Sym *sym);
  std::optional<const std::size_t>
  resolveUpvalue(Compiler &caller, const sexpr::Sym *sym);
  std::size_t addUpvalue(int idx, bool isLocal);
  bool isVariadic();
  uint8_t countArity();

  template <typename T> code::InstrPtr emitCode(T v) {
    return code.pushCode(v, curSrcLoc.row);
  }
  template <typename T, typename... Args>
  code::InstrPtr emitCode(T first, Args... args) {
    const auto idx = emitCode(first);
    emitCode(args...);
    return idx;
  }
  code::InstrPtr emitConst(const sexpr::SExpr *sExpr);
  void patchJump(const code::InstrPtr idx);

  const sexpr::SExpr *last(const sexpr::SExpr *sExpr);
  unsigned int visitEach(const sexpr::SExpr *sExpr, Visitor visitor);
  void traverse(const sexpr::SExpr *sExpr, Visitor visitor);
  void compileStmts(const sexpr::SExpr *sExpr);
  void compileExprs(const sexpr::SExpr *sExpr);
  void compileStmt(const sexpr::SExpr *sExpr);
  void compileExpr(const sexpr::SExpr *sExpr);
  void compileAtom(const sexpr::Atom *atom);
  void compileCall(const sexpr::SExprs *sExprs);
  void emitDef(const MatchedSExpr<sexpr::SExpr> matched);
  void emitSet(const MatchedSExpr<sexpr::SExpr> matched);
  void emitSym(const sexpr::Sym *sym);
  void emitQuote(const MatchedSExpr<sexpr::SExpr> matched);
  void emitIf(const MatchedSExpr<sexpr::SExpr> matched);
  void emitLambda(const MatchedSExpr<sexpr::SExpr> matched);
  void emitRet();
  void execDefMacro(const MatchedSExpr<sexpr::SExpr> matched);
  const sexpr::SExpr *execMacro(const sexpr::SExpr *macro);

  void handleInvalidDef();
  void handleTypeError(
      const std::string grammar,
      const std::string expected,
      const sexpr::SExpr *actual
  );

public:
  Compiler(std::vector<std::string> source, runtime::VM &vm);

  const sexpr::Prototype *compile();

  static void verifyLex(
      const std::string &line,
      const unsigned int lineNum,
      unsigned int &openParen,
      unsigned int &closedParen
  );
};

} // namespace compile

#endif

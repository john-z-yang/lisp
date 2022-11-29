#include "env/Env.hpp"
#include "env/functions.hpp"
#include "eval/EvalException.hpp"
#include "eval/eval.hpp"
#include "parse/ParseException.hpp"
#include "parse/parse.hpp"
#include "sexpr/Atom.hpp"
#include "sexpr/BoolAtom.hpp"
#include "sexpr/ClosureAtom.hpp"
#include "sexpr/IntAtom.hpp"
#include "sexpr/NilAtom.hpp"
#include "sexpr/SExpr.hpp"
#include "sexpr/SymAtom.hpp"
#include "sexpr/cast.cpp"
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

int repl() {
  auto env = std::make_shared<Env>();
  initEnv(env);

  while (true) {
    std::string input;
    size_t linesRead = 0;
    try {
      if (getInput(std::cin, input, linesRead, "lisp> ", "  ... ")) {
        eval(parse(input), env, [](std::shared_ptr<SExpr> res) {
          std::cout << *res << std::endl;
          return nullptr;
        });
      } else {
        std::cout << std::endl;
        lispQuit(env);
      }
    } catch (ParseException &pe) {
      std::cerr << "In line " << linesRead << " of <std::cin>" << std::endl;
      std::cerr << pe;
    } catch (EvalException &ee) {
      std::cerr << "In line " << linesRead << " of <std::cin>" << std::endl;
      std::cerr << ee;
    }
  }
  env->clear();
  return EXIT_FAILURE;
}

int repl(const std::string fileName) {
  std::fstream fs;
  fs.open(fileName, std::fstream::in);

  if (fs.fail()) {
    std::cerr << "Unable to open file \"" << fileName
              << "\": " << strerror(errno) << std::endl;
    return EXIT_FAILURE;
  }

  auto env = std::make_shared<Env>();
  initEnv(env);

  size_t linesRead = 0;
  while (true) {
    std::string input;
    try {
      if (getInput(fs, input, linesRead, "", "")) {
        eval(parse(input), env,
             [](std::shared_ptr<SExpr> _) { return nullptr; });
      } else {
        break;
      }
    } catch (ParseException &pe) {
      std::cerr << "In line " << linesRead << " of \"" << fileName << "\""
                << std::endl;
      std::cerr << pe;
    } catch (EvalException &ee) {
      std::cerr << "In line " << linesRead << " of \"" << fileName << "\""
                << std::endl;
      std::cerr << ee;
    }
  }
  env->clear();
  return EXIT_SUCCESS;
}

int main(int argc, char const *argv[]) {
  if (argc == 1) {
    return repl();
  } else if (argc == 2) {
    return repl(argv[1]);
  }
  return 0;
}
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
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::fstream;
using std::make_shared;
using std::strerror;

int repl() {
  shared_ptr<Env> env = make_shared<Env>();
  initEnv(env);
  while (true) {
    string input;
    size_t linesRead = 0;
    try {
      if (getInput(cin, input, linesRead, "lisp> ", "  ... ")) {
        cout << *eval(parse(input), env) << endl;
      } else {
        cout << endl;
        lispQuit(nullptr);
      }
    } catch (ParseException pe) {
      cerr << "In line " << linesRead << " of <std::cin>" << endl;
      cerr << pe;
    } catch (EvalException ee) {
      cerr << "In line " << linesRead << " of <std::cin>" << endl;
      cerr << ee;
    }
  }
  return EXIT_FAILURE;
}

int repl(const string fileName) {
  fstream fs;
  fs.open(fileName, fstream::in);

  if (fs.fail()) {
    cerr << "Unable to open file \"" << fileName << "\": " << strerror(errno)
         << endl;
    return EXIT_FAILURE;
  }

  shared_ptr<Env> env = make_shared<Env>();
  initEnv(env);

  size_t linesRead = 0;
  while (true) {
    string input;
    try {
      if (getInput(fs, input, linesRead, "", "")) {
        *eval(parse(input), env);
      } else {
        break;
      }
    } catch (ParseException pe) {
      cerr << "In line " << linesRead << " of \"" << fileName << "\"" << endl;
      cerr << pe;
      return EXIT_FAILURE;
    } catch (EvalException ee) {
      cerr << "In line " << linesRead << " of \"" << fileName << "\"" << endl;
      cerr << ee;
      return EXIT_FAILURE;
    }
  }
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
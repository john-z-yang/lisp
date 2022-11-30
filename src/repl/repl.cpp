#include "repl.hpp"
#include "../env/functions.hpp"
#include "../eval/EvalException.hpp"
#include "../eval/eval.hpp"
#include "../parse/ParseException.hpp"
#include "../parse/parse.hpp"
#include <cstring>
#include <fstream>
#include <iostream>
#include <memory>
#include <readline/history.h>
#include <readline/readline.h>
#include <regex>
#include <string>

bool getInput(std::string &str, size_t &linesRead, std::string prompt,
              std::string wrap) {
  uint32_t openParen = 0;
  uint32_t closedParen = 0;
  std::string line;
  while (auto buf = readline(linesRead == 0 ? prompt.c_str() : wrap.c_str())) {
    line = std::string(buf);
    free(buf);
    line = std::regex_replace(
        line, std::regex("(\\\\\"|\"(?:\\\\\"|[^\"])*\")|(;.*$)"), "$1");
    if (str.empty() && line.empty()) {
      continue;
    }
    linesRead += 1;
    add_history(line.c_str());
    verifyLex(line, openParen, closedParen);
    str += line + " ";
    if (openParen == closedParen) {
      return true;
    }
  }
  return false;
}

std::istream &getInput(std::istream &in, std::string &str, size_t &linesRead) {
  uint32_t openParen = 0;
  uint32_t closedParen = 0;
  std::string line;
  while (getline(in, line)) {
    linesRead += 1;
    line = std::regex_replace(
        line, std::regex("(\\\\\"|\"(?:\\\\\"|[^\"])*\")|(;.*$)"), "$1");
    if (str.empty() && line.empty()) {
      continue;
    }
    verifyLex(line, openParen, closedParen);
    str += line + " ";
    if (openParen == closedParen) {
      return in;
    }
  }
  return in;
}

void printInfo() {
  std::cout << "Lisp (C++ std: " << __cplusplus << ", " << __DATE__ << ", "
            << __TIME__ << ")" << std::endl;
  std::cout << "Type \"(quit)\" or trigger EOF to exit the session."
            << std::endl;
}

int repl(std::shared_ptr<Env> env) {
  printInfo();
  while (true) {
    std::string input;
    size_t linesRead = 0;
    try {
      if (getInput(input, linesRead, "lisp> ", "  ... ")) {
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
  return EXIT_FAILURE;
}

int repl(const std::string filePath, std::shared_ptr<Env> env) {
  std::fstream fs;
  fs.open(filePath, std::fstream::in);

  if (fs.fail()) {
    std::cerr << "Unable to open file \"" << filePath
              << "\": " << strerror(errno) << std::endl;
    return EXIT_FAILURE;
  }

  size_t linesRead = 0;
  while (true) {
    std::string input;
    try {
      if (getInput(fs, input, linesRead)) {
        eval(parse(input), env,
             [](std::shared_ptr<SExpr> res) { return nullptr; });
      } else {
        break;
      }
    } catch (ParseException &pe) {
      std::cerr << "In line " << linesRead << " of \"" << filePath << "\""
                << std::endl;
      std::cerr << pe;
    } catch (EvalException &ee) {
      std::cerr << "In line " << linesRead << " of \"" << filePath << "\""
                << std::endl;
      std::cerr << ee;
    }
  }
  return EXIT_SUCCESS;
}

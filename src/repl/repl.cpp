#include "repl.hpp"
#include "../code/OpCode.hpp"
#include "../compile/Compiler.hpp"
#include "../compile/SyntaxError.hpp"
#include "../compile/parse.hpp"
#include "../vm/RuntimeException.hpp"
#include "../vm/VM.hpp"
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

int repl() {
  printInfo();
  Env globals;
  while (true) {
    std::string input;
    size_t linesRead = 0;
    try {
      if (getInput(input, linesRead, "lisp> ", "  ... ")) {
        Compiler compiler(parse(input));
        auto main = compiler.compile();
        std::cout << std::endl;
        main->dissassemble(std::cout);
        VM vm(main, globals);
        std::cout << ">> " << *vm.exec() << std::endl;
      } else {
        std::cout << std::endl;
        exit(0);
      }
    } catch (SyntaxError &se) {
      std::cerr << "In line " << linesRead << " of <std::cin>" << std::endl;
      std::cerr << se;
    } catch (RuntimeException &ee) {
      std::cerr << "In line " << linesRead << " of <std::cin>" << std::endl;
      std::cerr << ee;
    }
  }
  return EXIT_FAILURE;
}

int repl(const std::string fileName) { return 0; }
#include "repl.hpp"
#include "../compile/Compiler.hpp"
#include "../compile/SyntaxError.hpp"
#include "../compile/parse.hpp"
#include "../vm/RuntimeException.hpp"
#include "../vm/VM.hpp"
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <readline/history.h>
#include <readline/readline.h>
#include <regex>
#include <string>

bool getConsoleInput(std::vector<std::string> &lines, std::string prompt,
                     std::string wrap) {
  uint32_t openParen = 0;
  uint32_t closedParen = 0;
  std::string line;
  while (auto buf = readline(lines.empty() ? prompt.c_str() : wrap.c_str())) {
    line = std::string(buf);
    free(buf);
    line = std::regex_replace(
        line, std::regex("(\\\\\"|\"(?:\\\\\"|[^\"])*\")|(;.*$)"), "$1");
    if (lines.empty() && line.empty()) {
      continue;
    }
    add_history(line.c_str());
    verifyLex(line, openParen, closedParen);
    lines.push_back(line + " ");
    if (openParen == closedParen) {
      return true;
    }
  }
  return false;
}

std::istream &getFileInput(std::istream &in, std::vector<std::string> &lines) {
  uint32_t openParen = 0;
  uint32_t closedParen = 0;
  std::string line;
  while (getline(in, line)) {
    line = std::regex_replace(
        line, std::regex("(\\\\\"|\"(?:\\\\\"|[^\"])*\")|(;.*$)"), "$1");
    if (line.empty()) {
      continue;
    }
    lines.push_back(line + " ");
    verifyLex(line, openParen, closedParen);
    if (openParen == closedParen) {
      return in;
    }
  }
  return in;
}

int execFile(const std::string filePath, VM &vm) {
  std::fstream fs;
  fs.open(filePath, std::fstream::in);

  if (fs.fail()) {
    std::cerr << "Unable to open file \"" << filePath
              << "\": " << strerror(errno) << std::endl;
    return EXIT_FAILURE;
  }

  while (true) {
    std::vector<std::string> lines;
    try {
      if (getFileInput(fs, lines)) {
        Compiler compiler(lines);
        auto main = compiler.compile();
        vm.exec(main);
      } else {
        break;
      }
    } catch (SyntaxError &se) {
      std::cerr << "In line " << lines.size() << " of \"" << filePath << "\""
                << std::endl;
      std::cerr << se;
    } catch (RuntimeException &re) {
      std::cerr << "In line " << lines.size() << " of \"" << filePath << "\""
                << std::endl;
      std::cerr << re;
    }
  }
  return EXIT_SUCCESS;
}

void loadLib(VM &vm) {
  const std::string LIB_DIR_ENV_VAR = "LISP_LIB_ENV";
  if (const char *env_p = std::getenv(LIB_DIR_ENV_VAR.c_str())) {
    for (const auto &entry : std::filesystem::directory_iterator(env_p)) {
      execFile(std::string(entry.path()), vm);
    }
  } else {
    std::cerr << std::endl
              << "WARNING: " << LIB_DIR_ENV_VAR << " is not set. Set the "
              << LIB_DIR_ENV_VAR
              << " environment variable to the predefined functions libaray "
              << "(usually lisp/lib)." << std::endl
              << std::endl;
  }
}

void printInfo() {
  std::cout << "Lisp (C++ std: " << __cplusplus << ", " << __DATE__ << ", "
            << __TIME__ << ")" << std::endl;
  std::cout << "Type \"(quit)\" or trigger EOF to exit the session."
            << std::endl;
}

int repl() {
  printInfo();

  VM vm;
  loadLib(vm);

  while (true) {
    std::vector<std::string> lines;
    try {
      if (getConsoleInput(lines, "lisp> ", "  ... ")) {
        Compiler compiler(lines);
        auto main = compiler.compile();
        main->dissassemble(std::cout);
        std::cout << ">> " << *vm.exec(main) << std::endl;
      } else {
        std::cout << std::endl << "Farewell." << std::endl;
        return EXIT_SUCCESS;
      }
    } catch (SyntaxError &se) {
      std::cerr << "In line " << lines.size() << " of <std::cin>" << std::endl;
      std::cerr << se;
    } catch (RuntimeException &ee) {
      std::cerr << "In line " << lines.size() << " of <std::cin>" << std::endl;
      std::cerr << ee;
    }
  }
  return EXIT_FAILURE;
}

int repl(const std::string filePath) {
  VM vm;
  loadLib(vm);
  return execFile(filePath, vm);
}

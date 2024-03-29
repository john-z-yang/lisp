#include "repl.hpp"
#include "../compile/CodeGenerator.hpp"
#include "../compile/Parser.hpp"
#include "../error/RuntimeError.hpp"
#include "../error/SyntaxError.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/Casting.hpp"
#include "../sexpr/Nil.hpp"
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <readline/history.h>
#include <readline/readline.h>
#include <regex>
#include <set>
#include <string>

using namespace repl;
using namespace sexpr;
using namespace compile;
using namespace runtime;

bool getConsoleInput(
    std::vector<std::string> &lines, std::string prompt, std::string wrap
) {
  auto openParen = 0U;
  auto closedParen = 0U;
  std::string line;
  while (auto buf = readline(lines.empty() ? prompt.c_str() : wrap.c_str())) {
    line = std::string(buf);
    free(buf);
    line = std::regex_replace(
        line, std::regex("(\\\\\"|\"(?:\\\\\"|[^\"])*\")|(;.*$)"), "$1"
    );
    if (lines.empty() && line.empty()) {
      continue;
    }
    add_history(line.c_str());
    Parser::verifyLex(line, lines.size() + 1, openParen, closedParen);
    lines.push_back(line + " ");
    if (openParen == closedParen) {
      return true;
    }
  }
  return false;
}

bool getFileInput(std::istream &in, std::vector<std::string> &lines) {
  auto openParen = 0U;
  auto closedParen = 0U;
  std::string line;
  while (getline(in, line)) {
    line = std::regex_replace(
        line, std::regex("(\\\\\"|\"(?:\\\\\"|[^\"])*\")|(;.*$)"), "$1"
    );
    Parser::verifyLex(line, lines.size() + 1, openParen, closedParen);
    lines.push_back(line + " ");
  }
  return lines.size() > 0;
}

int execFile(const std::string filePath, VM &vm) {
  std::fstream fs;
  fs.open(filePath, std::fstream::in);

  if (fs.fail()) {
    std::cerr << "Unable to open file \"" << filePath
              << "\": " << strerror(errno) << std::endl;
    return EXIT_FAILURE;
  }

  std::vector<std::string> lines;
  try {
    if (getFileInput(fs, lines)) {
      {
        Parser parser(vm, lines);
        auto parsedSrc = parser.getParsed();
        CodeGenerator codeGenerator(vm, parsedSrc);
        auto main = codeGenerator.getGenerated();

        vm.load(main);
      }
      vm.eval();
    }
  } catch (error::SyntaxError &se) {
    std::cerr << "In \"" << filePath << "\"" << std::endl << se << std::endl;
  } catch (error::RuntimeError &re) {
    std::cerr << "In \"" << filePath << "\"" << std::endl << re << std::endl;
  }

  return EXIT_SUCCESS;
}

void loadLib(VM &vm) {
  const std::string LIB_DIR_ENV_VAR = "LISP_LIB_ENV";
  if (const char *env_p = std::getenv(LIB_DIR_ENV_VAR.c_str())) {
    std::set<std::filesystem::path> paths;
    for (const auto &entry : std::filesystem::directory_iterator(env_p)) {
      paths.insert(entry.path());
    }
    for (const auto &path : paths) {
      execFile(std::string(path), vm);
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

int repl::repl() {
  printInfo();

  VM vm;
  loadLib(vm);

  while (true) {
    std::vector<std::string> lines;
    try {
      if (getConsoleInput(lines, "lisp> ", "  ... ")) {
        {
          Parser parser(vm, lines);
          auto parsedSrc = parser.getParsed();
          CodeGenerator codeGenerator(vm, parsedSrc);
          auto main = codeGenerator.getGenerated();

          vm.load(main);
        }
        const auto res = vm.eval();
        if (!isa<Nil>(res)) {
          std::cout << *res << std::endl;
        }
      } else {
        std::cout << std::endl << "Farewell." << std::endl;
        return EXIT_SUCCESS;
      }
    } catch (const error::SyntaxError &se) {
      std::cerr << "In <std::cin>" << std::endl << se << std::endl;
    } catch (const error::RuntimeError &re) {
      std::cerr << "In <std::cin>" << std::endl << re << std::endl;
    }
  }
  return EXIT_FAILURE;
}

int repl::repl(const std::string filePath) {
  VM vm;
  loadLib(vm);
  return execFile(filePath, vm);
}

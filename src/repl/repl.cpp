#include "repl.hpp"
#include "../env/functions.hpp"
#include "../eval/EvalException.hpp"
#include "../eval/eval.hpp"
#include "../parse/ParseException.hpp"
#include "../parse/parse.hpp"
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

int repl(std::shared_ptr<Env> env) {
  while (true) {
    std::string input;
    size_t linesRead = 0;
    try {
      if (getInput(std::cin, input, linesRead, "lisp> ", "  ... ")) {
        std::cout << *eval(parse(input), env) << std::endl;
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
      if (getInput(fs, input, linesRead, "", "")) {
        *eval(parse(input), env);
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

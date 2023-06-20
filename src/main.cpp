#include "repl/repl.hpp"
#include <iostream>
#include <memory>
#include <string>

int main(int argc, char const *argv[]) {
  auto exitCode = EXIT_FAILURE;
  if (argc == 1) {
    exitCode = repl::repl();
  } else if (argc == 2) {
    exitCode = repl::repl(argv[1]);
  }
  return exitCode;
}
